package Critic::SQLClassifier::UI;

# system to resolve classes with  each item, and take a set theoretic
# approach to fixing them

use Critic::SQLClassifier::Categorizer;
# use Critic::SQLClassifier::Classes;

use Manager::Dialog qw(QueryUser Approve Message SubsetSelect);

use Data::Dumper;
use Text::Wrap;
use XML::Twig;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / MyCategorizer Entry Entries Contents PreviousEntries
   Properties Classes ContentField StorageFile EntryHash /

  ];

sub init {
  my ($self,%args) = @_;
  $self->ContentField($args{ContentField} || "Contents");
  $self->StorageFile($args{StorageFile} || "data/classes.log");
  if (! defined $self->MyCategorizer) {
    $self->MyCategorizer
      (Critic::SQLClassifier::Categorizer->new
       (UI => $self));
  }
  $self->EntryHash({}); 	# skip messages that have already been classified
}

sub InteractivelyClassify {
  my ($self,%args) = @_;
  print "TESTING...\n";
  my @commands = qw(save ignore correct next_entry previous_entry
                  next_unclassified_entry rebuild_index add_class_type
                  rename_class_type remove_class_type autoclassify
                  save_and_exit list_class_members ****************);

  my %sizes;
  $self->PreviousEntries([]);
  $self->Entry(shift @{$self->Entries});
  my $count = 0;
  while (@{$self->Entries}) {
    # clear screen
    system "clear";
    ++$count;
    if (!($count % 25)) {
      $self->Save();
      $self->MyCategorizer->RebuildIndex;
      $count = 0;
    }

    $size{commands} = scalar @commands;
    $size{classes} = scalar @{$self->Classes};
    my @menu = @commands;
    push @menu, @{$self->Classes};

    # Preclassify the entry if information is available
    if (((! exists $self->Properties->{$self->Entry}) or
	 (! scalar keys %{$self->Properties->{$self->Entry}})) and
	(scalar keys %{$self->MyCategorizer->Categories})) {
      $self->ClassifyEntry
	(Entry => $self->Entry,
	 Method => "manual");
    }

    $self->PrintEntry
      (Entry => $self->Entry);

    my $choice = Choose(@menu);
    if ($choice >= 0) {
      if ($choice < $size{commands}) {
	my $command = $commands[$choice];
	if ($command eq "next_entry") {
	  $self->Advance;
	} elsif ($command eq "next_unclassified_entry") {
	  do {
	    $self->Advance;
	  } while ((exists $self->Properties->{$self->Entry}) and
		   ((scalar keys %{$self->Properties->{$self->Entry}})));
	} elsif ($command eq "ignore") {
	  $self->Properties->{$self->Entry}->{"ignore"} = ! $self->Properties->{$self->Entry}->{"ignore"};
	  $self->Advance;
	} elsif ($command eq "correct") {
	  $self->Properties->{$self->Entry}->{"correct"} = ! $self->Properties->{$self->Entry}->{"correct"};
	  $self->Advance;
	} elsif ($command eq "save") {
	  $self->Save();
	} elsif ($command eq "save_and_exit") {
	  $self->Save();
	  exit(0);
	} elsif ($command eq "list_class_members") {
	  $self->ListClassMembers;
	} elsif ($command eq "autoclassify") {
	  $self->AutomaticallyClassifyAllEntries;
	} elsif ($command eq "previous_entry") {
	  if (scalar @{$self->PreviousEntries}) {
	    unshift @{$self->Entries},$self->Entry;
	    $self->Entry(pop @{$self->PreviousEntries});
	  } else {
	    print "Previous entries empty\n";
	  }
	} elsif ($command eq "add_class_type") {
	  push @{$self->Classes}, $self->GetClassName;
	  $self->MyCategorizer->RebuildIndex;
	} elsif ($command eq "rename_class_type") {
	  $self->RenameClass;
	  $self->MyCategorizer->RebuildIndex;
	} elsif ($command eq "remove_class_type") {
	  $self->RemoveClass;
	  $self->MyCategorizer->RebuildIndex;
	} elsif ($command eq "rebuild_index") {
	  $self->MyCategorizer->RebuildIndex;
	}
      } elsif ($choice < $size{commands} + $size{classes}) {
	my $class = $self->Classes->[$choice-$size{commands}];
	if (defined ($self->Properties->{$self->Entry}->{$class})) {
	  delete $self->Properties->{$self->Entry}->{$class};
	} else {
	  $self->Properties->{$self->Entry}->{$class} = 1;
	}
      }
    }
  }
}

sub ListClassMembers {
  my ($self,%args) = @_;
  my $search = [];
  my @classes = SubsetSelect
    (Set => $self->Classes,
     Selection => {});
  foreach my $entry (@{$self->GetAllEntries}) {
    foreach my $class (@classes) {
      if (exists $self->Properties->{$entry}->{$class}) {
	print sprintf("%40s\t%s\n",$class,$self->Contents->{$entry}->{$self->ContentField});
      }
    }
  }
}

sub PrintEntry {
  my ($self,%args) = @_;
  print "#" x 80;
  print "\n";
  my $contents = $self->Contents->{$args{Entry}}->{$self->ContentField};
  print "<<<".wrap('','',$contents).">>>\n";
  print Dumper($self->Properties->{$args{Entry}});
  print "\n";
}

sub GetAllEntries {
  my ($self,%args) = @_;
  my @all;
  push @all, @{$self->PreviousEntries || []};
  push @all, $self->Entry if $self->Entry;
  push @all, @{$self->Entries};
  return \@all;
}

sub RenameClass {
  my ($self,%args) = @_;
  print "Rename which class?\n";
  my $response = Choose(@{$self->Classes});
  if ($response >= 0 and $response < scalar @{$self->Classes}) {
    my $newname = $self->GetClassName;
    my $class = $self->Classes->[$response];
    if (Approve ("Rename <$class> to <$newname>?")) {
      foreach my $entry (@{$self->GetAllEntries}) {
	if (exists $self->Properties->{$entry}->{$class}) {
	  $self->Properties->{$entry}->{$newname} = $self->Properties->{$entry}->{$class};
	  delete $self->Properties->{$entry}->{$class};
	}
      }
      # now update the classes
      splice(@{$self->Classes},$response,1);
      push @{$self->Classes},$newname;
      my @classes = sort @{$self->Classes};
      $self->Classes(\@classes);
    }
  }
}

sub RemoveClass {
  my ($self,%args) = @_;
  print "Remove which class?\n";
  my $response = Choose(@{$self->Classes});
  if ($response >= 0 and $response < scalar @{$self->Classes}) {
    my $newname = $self->GetClassName;
    my $class = $self->Classes->[$response];
    if (Approve ("Remove <$class>?")) {
      foreach my $entry (@{$self->GetAllEntries}) {
	if (exists $self->Properties->{$entry}->{$class}) {
	  delete $self->Properties->{$entry}->{$class};
	}
      }
      splice(@{$self->Classes},$response,1);
    }
  }
}

sub Save {
  my ($self) = (shift);
  my $LOG;
  my $storagefile = $self->StorageFile;
  Message(Message => "Saving...");
  open(LOG,">$storagefile") or die "can't open $storagefile\n";
  print LOG Dumper($self->Properties);
  close(LOG);
  Message(Message => "Done");
}

sub Load {
  my ($self,%args) = @_;
  Message(Message => "Loading Corpus Entries...");
  my $storagefile = $self->StorageFile;
  my $properties;
  if (-f $storagefile) {
    my $c = `cat $storagefile`;
    $properties = eval $c;
  } else {
    $properties = {};
  }
  $self->Properties($properties);
  $self->Entries($args{Entries});
  my $classes = {};
  foreach my $key (keys %{$self->Properties}) {
    foreach my $class (keys %{$self->Properties->{$key}}) {
      $classes->{$class} = 1;
    }
  }
  $self->Classes([sort keys %$classes]);
  Message(Message => "Done Loading Corpus Entries.");
}

sub Advance {
  my $self = shift;
  if (scalar @{$self->Entries}) {
    push @{$self->PreviousEntries}, $self->Entry;
    $self->Entry(shift @{$self->Entries});
  } else {
    print "Entries empty\n";
  }
}

sub Choose {
  my @list = @_;
  my $i = 0;
  if (!@list) {
    return;
  } elsif (@list == 1) {
    print "<Chose:".$list[0].">\n";
    return $list[0];
  } else {
    foreach my $item (@list) {
      print "$i) $item\n";
      ++$i;
    }
    my $response;
    while (defined ($response = <STDIN>) and ($response !~ /^\d+$/)) {
    }
    chomp $response;
    return $response;
  }
}

sub GetClassName {
  my ($self,%args) = @_; 
  my $name = QueryUser("What should the name of this class be?");
  $name =~ s/[^\w]/-/g;
  return $name;
}

sub HasProperties {
  my ($self,%args) = @_;
  if ((exists $self->Properties->{$args{Entry}}) and
      (scalar keys %{$self->Properties->{$args{Entry}}})) {
    return 1;
  } else {
    return 0;
  }
}

sub AutomaticallyClassifyAllEntries {
  my ($self,%args) = @_;
  foreach my $entry (@{$self->GetAllEntries}) {
    if (! $self->HasProperties(Entry => $entry)) {
      $self->ClassifyEntry
	(Entry => $entry,
	 Method => "automatic");
    }
  }
}

sub ClassifyEntry {
  my ($self,%args) = @_;
  my $entry = $args{Entry};
  $self->PrintEntry
    (Entry => $entry) unless $args{Silent};
  my $hash = $self->MyCategorizer->ClassifyEntry
    (Entry => $entry);
  print Dumper($hash) unless $args{Silent};
  my @keys;
  if (exists $args{Method} and $args{Method} eq "manual") {
    foreach my $key (sort {$hash->{$b} <=> $hash->{$a}} keys %$hash) {
      push @keys, $key;
      if ($hash->{$key} > 0.05) {
	print sprintf("%40s\t%f\n",$key,$hash->{$key}) unless $args{Silent};
	if ($hash->{$key} > 0.2) {
	  $self->Properties->{$entry}->{$key} = 1;
	}
      }
    }
    my @choices = SubsetSelect(Set => \@keys,
			       Selection => $self->Properties->{$entry});
    my $h2 = {};
    foreach my $choice (@choices) {
      $h2->{$choice} = 1;
    }
    $self->Properties->{$entry} = $h2;
  } elsif (exists $args{Method} and $args{Method} eq "automatic") {
    # automatically classify
    my $best;
    if (keys %$hash) {
      foreach my $key (sort {$hash->{$b} <=> $hash->{$a}} keys %$hash) {
	if (! $best) {
	  $best = $key;
	}
	if ($hash->{$key} > 0.2) {
	  print sprintf("%40s\t%f\n",$key,$hash->{$key}) unless $args{Silent};
	  $self->Properties->{$entry}->{$key} = 1;
	}
      }
      if (! $self->HasProperties(Entry => $entry)) {
	# print Dumper([[[$entry,$best]]]);
	$self->Properties->{$entry}->{$best} = 1;
	print sprintf("%40s\t%f\n",$best,$hash->{$best}) unless $args{Silent};
      }
    } else {
      # print sprintf("%40s\t%f\n",$best,$hash->{$best}) unless $args{Silent};
    }
  }
  return $hash;
}

1;
