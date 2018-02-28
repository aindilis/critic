package Critic;

use BOSS::Config;
use Critic::Item;
use KBFS::Cache;
use MyFRDCSA;
use PerlLib::SourceManager;

use Event qw(loop unloop);
use Term::ReadKey;
use Data::Dumper;
use XML::Twig;
use File::stat;
use Carp;

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw  / Config MySourceManager Cache ReviewCount  CacheDir TargetDir
   ItemFile Items Playlist CurrentItem CurrentFile /

  ];

sub init {
  my ($self,%args) = @_;
  $specification = "
	-U [<sources>...]	Update sources
	-l [<sources>...]	Load sources
	-s [<sources>...]	Search sources
	-c [<sources>...]	Choose sources

	-a <archive>		Select archive
	-r			Review
";
  $UNIVERSAL::systemdir = ConcatDir(Dir("internal codebases"),"critic");
  $self->Config(BOSS::Config->new
		(Spec => $specification,
		 ConfFile => ""));
  my $conf = $self->Config->CLIConfig;
  if (exists $conf->{'-u'}) {
    $UNIVERSAL::agent->Register
      (Host => defined $conf->{-u}->{'<host>'} ?
       $conf->{-u}->{'<host>'} : "localhost",
       Port => defined $conf->{-u}->{'<port>'} ?
       $conf->{-u}->{'<port>'} : "9000");
  }
  $self->MySourceManager
    (PerlLib::SourceManager->new
     (System => "Critic",
      Conf => $conf,
      Type => "Item"));
}

sub Execute {
  my ($self,%args) = (shift,@_);
  my $conf = $self->Config->CLIConfig;
  if (exists $conf->{'-U'}) {
    $self->MySourceManager->UpdateSources
      (Sources => $conf->{'-U'});
  }
  if (exists $conf->{'-l'}) {
    $self->MySourceManager->LoadSources
      (Sources => $conf->{'-l'});
  }
  if (exists $conf->{'-s'}) {
    $self->MySourceManager->Search
      (Sources => $conf->{-s});
  }
  if (exists $conf->{'-c'}) {
    $self->MySourceManager->Choose
      (Sources => $conf->{-c});
  }
  if (exists $conf->{'-u'}) {
    # enter in to a listening loop
    while (1) {
      $UNIVERSAL::agent->Listen(TimeOut => 10);
    }
  }
  if (exists $conf->{'-w'}) {
    Message(Message => "Press any key to quit...");
    my $t = <STDIN>;
  }
  if (exists $conf->{'-r'}) {
    my $archive = $conf->{'-a'} || "1";
    $self->Cache
      (KBFS::Cache->new
       (CacheType => $args{CacheType} || "original",
	CacheDir => $args{CacheDir} || "data/$archive/mycache"));
    $self->TargetDir($args{TargetDir} || "data/$archive/cache");
    $self->ReviewCount(0);
    $self->ItemFile($args{ItemFile} || "data/$archive/mycache/.item");

    # if cache is empty, populate it
    $self->PopulateCache;
    if (! $self->Cache->ListContents) {
      $self->Cache->ExportMetadata;
    }
    $self->ReadItemData;
    $self->Review;
  }
}

sub PopulateCache {
  my ($self,%args) = @_;
  my $query = "find ".$self->TargetDir." -follow | grep -iE \"\.(mpg|mpeg|avi|mov|wmv|asf)\$\"";
  foreach my $file (split /\n/, `$query`) {
    chomp $file;
    if (-f $file) {
      if (! $self->Cache->Contains($file)) {
	print "New File: $file\n";
	$self->Cache->CacheNewItem(URI => $file);
      }
    }
  }
}

sub ReadItemData {
  my ($self,%args) = @_;
  if (-f $self->ItemFile.".out" ) {
    # here is our previous version of it, so lets copy it over
    if (-f $self->ItemFile) {
      # only if we should
      my $i1 = stat($self->ItemFile.".out");
      my $i2 = stat($self->ItemFile);
      if ($i1->size >= $i2->size) {
	system "cp ".$self->ItemFile.".out ".$self->ItemFile;
      }
    } else {
      system "cp ".$self->ItemFile.".out ".$self->ItemFile;
    }
  }
  if (-f $self->ItemFile) {
    # read it in with data dumper
    my $query = "cat ".$self->ItemFile;
    $self->Items(eval `$query`);
  } else {
    # it doesn't so ignore it, it will be written when we close
    # create a new item for every item in the cache
    my $items = {};
    foreach my $item ($self->Cache->ListContents) {
      my $cacheitem = $self->ItemtoCacheItem($item);
      print "<<<$cacheitem>>>\n";
      print $cacheitem->CID."\n";
      $items->{$cacheitem->CID} =
	Critic::Item->new
	    (CID => $cacheitem->CID);
    }
    $self->Items($items);
  }
  $self->ComputeSimilarityMatrix;
}

sub WriteItemData {
  my ($self,%args) = @_;
  my $OUT;
  open(OUT,">".$self->ItemFile.".out") or
    croak "Cannot open itemfile.\n";
  print OUT Dumper($self->Items);
  close(OUT);
}

sub ListItems {
  my ($self,%args) = @_;
  return values %{$self->Items};
}

sub PrintMenu {
  my ($self,%args) = @_;
  print "########################################\n";
  print "q\tquit\n";
  print "s\tskip directory\n";
  print "n\tnext file\n";
  print "p\tprevious file\n";
  print "[0-9]\trate file\n";
  print "########################################\n";
}


sub CompareItems {
  my $x = $a;
  my $y = $b;
  my $rv = xyCompare($b,$a);
  if (abs($rv) <= 1) {
    return $rv;
  }
  $rv = xyCompare($a,$b);
  if (abs($rv) <= 1) {
    return -1 * $rv;
  }
  return 0;
}

sub xyCompare {
  my ($x, $y) = (shift,shift);
  my $prior = 5;
  my $xexp = $x->ExpectedAdjustedRating;
  my $yexp = $y->ExpectedAdjustedRating;
  if (! $x->Reviews) {
    if ($y->Reviews) {
      # compute an expect rating
      if ($yexp < $prior) {
	return 1;
      } else {
	return -1;
      }
    } else {
      return 0;
    }
  } else {
    if (! $y->Reviews) {
      if ($xexp > $prior) {
	return 1;
      } else {
	return -1;
      }
    } else {
      # we have both, we can go ahead and try to compare in a more sophisticated manner
      #if their ratings differ by less than 2 choose the one that has been seen less
      if (abs($xexp - $yexp) < 2) {
	if ($x->Reviews < $y->Reviews) {
	  return 1;
	} else {
	  return -1;
	}
      } else {
	# return the one with the higher expected rating
	if ($xexp > $yexp) {
	  return 1;
	} else {
	  return -1;
	}
      }
    }
  }
}

sub Resort {
  my ($self,%args) = @_;
  foreach my $item ($self->ListItems) {
    $rating{$item} = $item->ExpectedAdjustedRating;
  }
  my @playlist = sort {$rating{$b} <=> $rating{$a}} ($self->ListItems);
  $self->Playlist(\@playlist);
}

sub Review {
  my ($self,%args) = @_;
  # start the event loop
  $self->PrintMenu;
  $self->Resort;
  $self->StartItem;

  ReadMode('cbreak');
  Event->timer(interval=>0.1, cb=> sub { $self->KeystrokeTest } );
  my $ret = loop();

  ReadMode('normal');
}

sub KeystrokeTest {
  my ($self,%args) = @_;
  if (defined ($char = ReadKey(-1)) ) {
    $self->Command($char);
  } else {
    # no input was waiting
  }
}

sub ItemtoCacheItem {
  my ($self,$item) = (shift,shift);
  my $val = $self->Cache->Contents->{$item->CID};
  if (!$val) {
    print Dumper($item);
  }
  return $val;
}

sub StartItem {
  my ($self,%args) = @_;
  my $item = shift @{$self->Playlist};
  if ($item) {
    my $cacheitem = $self->ItemtoCacheItem($item);
    if ($cacheitem) {
      my $file = $cacheitem->URI;
      $self->CurrentFile($file);
      $self->CurrentItem($item);
      print "<exp: ".$item->ExpectedAdjustedRating."><reviews: ".$item->ReviewCount."><seen: ".$item->SeenCount.">\n";
      system "mplayer $file -srate 48000 -loop 0 -nosound >/dev/null 2>/dev/null &"; # -geometry 0:100
      $item->MarkStart;
    }
  } else {
    $self->WriteItemData;
  }
}

sub FinishItem {
  my ($self,%args) = @_;
  my $item = $self->CurrentItem;
  if ($item) {
    #system "killall mplayer";
    my $file = $self->CurrentFile;
    if ($file) {
      system "ps aux | grep $file | awk '{print $2}' | xargs kill -9";
    }
    $self->CurrentItem(0);
    $self->CurrentFile(0);
    $item->MarkFinish(Value => $args{Value});
  }
}

sub Command {
  my ($self,$cmd) = (shift,shift);
  chomp $cmd;
  if ($cmd =~ /^([0-9]+)?$/) {
    $self->FinishItem(Value => $cmd);
    $self->StartItem;
  } elsif ($cmd =~ /x/) {
    $self->FinishItem(Value => "");
    $self->WriteItemData;
    $self->Cache->ExportMetadata;
    exit(0);
  }
  my $count = $self->ReviewCount;
  ++$count;
  if (($count >= 20) && 1) {
    $self->Resort;
    $count = 0;
  }
  $self->ReviewCount($count);
}

sub ComputeSimilarityMatrix {
  my ($self,%args) = @_;
  # sort into directories
  my %directories;
  #print Dumper($self->ListItems);
  foreach $item ($self->ListItems) {
    my $cacheitem = $self->ItemtoCacheItem($item);
    if ($cacheitem) {
      my $uri = $cacheitem->URI;
      my $sub1 = $uri;
      #print "<$uri>\n";
      $sub1 =~ s/\/[^\/]*?$//;
      #print "<$sub1>\n";
      my $sub2 = $sub1;
      $sub2 =~ s/\/[^\/]*?$//;
      #print "<$sub2>\n";
      #print "\n";
      $directories{$sub1} = [] unless exists $directories{$sub1};
      $directories{$sub2} = [] unless exists $directories{$sub2};
      push @{$directories{$sub1}}, $item;
      push @{$directories{$sub2}}, $item;
    }
  }

  foreach my $dir (keys %directories) {
    my $itemlist = $directories{$dir};
    foreach my $item (@$itemlist) {
      print $item->CID . " ";
    }
    print "\n";
  }

  # now compute an item similarity matrix foreach directory
  foreach my $dir (keys %directories) {
    my $itemlist = $directories{$dir};
    foreach my $item1 (@$itemlist) {
      foreach my $item2 (@$itemlist) {
	if (defined $item1->Similarity->{$item2->CID}) {
	  if ($item1->Similarity->{$item2->CID}->{State} ne "Frozen") {
	    $item1->Similarity->{$item2->CID}->{Sim} += 0.25;
	  }
	} else {
	  $item1->Similarity->{$item2->CID} = {CID => $item2->CID,
					       Sim => 0.25,
					       State => "Liquid"};
	}
      }
    }
  }

  foreach my $dir (keys %directories) {
    my $itemlist = $directories{$dir};
    foreach my $item1 (@$itemlist) {
      foreach my $item2 (@$itemlist) {
	$item1->Similarity->{$item2->CID}->{State} = "Frozen";
      }
    }
  }
  foreach $item ($self->ListItems) {
    # freeze item
    #print Dumper($item->Similarity);
  }
}

1;
