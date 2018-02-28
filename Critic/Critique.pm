package Critic::Review;

sub Menu {
  print "########################################\n";
  print "q\tquit\n";
  print "s\tskip directory\n";
  print "n\tnext file\n";
  print "p\tprevious file\n";
  print "[0-9]\trate file\n";
  print "########################################\n";
}

sub Display {
  
  $dir = $playlist[$i];
  $dir =~ s|(.*)(/.*?)$|$1|;
  if ((defined $rating{$playlist[$i]}) && ($rating{$playlist[$i]} eq "s")) {
    $avoid{$dir} = 1;
  }

  # mechanism to skip over already rated files

  while ((defined $avoid{$dir}) || (defined $rating{$playlist[$i]})) {
    if (defined $avoid{$dir}) {
      $rating{$playlist[$i]} = "-10";
    } elsif ($rating{$playlist[$i]} eq "s") {
      $avoid{$dir} = 1;
    }
    ++$i;
    $dir = $playlist[$i];
    $dir =~ s|(.*)(/.*?)$|$1|;
  }

  $thisrating = "NA";
  if (defined $rating{$playlist[$i]}) {
    $thisrating = $rating{$playlist[$i]};
  }

  if (defined $playlist[$i]) {
    print "Rating:\t".$thisrating."\nName:\t".$playlist[$i]."\n";

    menu;
    see $playlist[$i];
  } else {
    print "No more files\n";
  }
}

sub Review {
  # we want to develop ratings that apply to individual media and directories
  my $response;
  local $i = 0;
  local $dir;

  play;
  while (defined($response = <>) && ($i >= 0) && ($i < @playlist)) {
    chomp $response;
    print "<<<$response>>>\n\n\n";
    if ($response =~ /^[0-9]+$/) {
      $rating{$playlist[$i]} = $response;
      ++$i;
    } elsif ($response =~ /^q$/) {
      if (confirm("Are you sure you want to exit?")) {
	# write ratings here
	write_ratings;
	exit(0);
      }
    } elsif ($response =~ /^s$/) {
      # rate the directory 0 and skip over it
      # obtain the directory name
      $rating{$playlist[$i]} = "s";
      $dir = $playlist[$i];
      $dir =~ s|(.*)(/.*?)$|$1|;
      $avoid{$dir} = 1;
    } elsif ($response =~ /^n$/) {
      ++$i;
    } elsif ($response =~ /^p$/) {
      --$i;
    }

    play;
  }
  unload_ratings;
}

sub Confirm {
  my ($self,%args) = (shift,@_);
  print "$message\n";
  while (1) {
    print "[y/n]: ";
    $response = <>;
    if ($response =~ /^[yY]$/) {
      return 1;
    } elsif ($response =~ /^[nN]$/) {
      return 0;
    }
  }
}

sub Message {
  my ($self,%args) = (shift,@_);
  foreach (@_) {
    print "$_.\n";
  }
}

sub TriRand {
  return int(rand 3) - 1;
}

sub RandSort {
  my @list = @_;
  sort { TriRand } @list;
}

sub See {
  my $file = shift;
  system "mplayer $file >/dev/null 2>/dev/null";
  #system "mplayer $file -loop 0 -fs -zoom";
}

sub View {
  # old fashion, just go by sorted
  @mylist = sort {($rating{$b}||0) <=> ($rating{$a}||0)} @playlist;
  while (@mylist) {
    my $r = $rating{$mylist[0]};
    my @newlist = ();
    # get all these and then randomize that
    while ($rating{$mylist[0]} eq $r) { push @newlist, shift @mylist }
    my @randomlist =  @newlist;
    foreach $file (@randomlist) {
      $self->See($file);
    }
  }
}

1;
