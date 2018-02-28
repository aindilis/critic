package Critic::SQLClassifier::Categorizer;

use AI::Categorizer;
use AI::Categorizer::Learner::NaiveBayes;
use AI::Categorizer::Learner::SVM;
use Data::Dumper;
use DBI;

use Class::MethodMaker new_with_init => 'new',
  get_set =>
  [

   qw / UI ClassifierDataLocation Categories Built KnowledgeSet
   Learner Documents Classifier /

  ];

sub init {
  my ($self,%args) = @_;
  $self->UI($args{UI});
  $self->Documents([]);
  $self->Categories({});
  # $self->ClassifierDataLocation($args{ClassifierDataLocation} || "data/classifier-data");
  # die "Need to know the index location." unless $self->ClassifierDataLocation;
  # $self->Classifier("SVM");
  $self->Classifier("NaiveBayes");
}

sub RebuildIndex {
  my ($self,%args) = @_;

  # create categories
  $self->Documents([]);
  $self->Categories({});
  foreach my $class (@{$self->UI->Classes}) {
    $self->Categories->{$class} =
      AI::Categorizer::Category->by_name(name => $class);
  }

  # load training "documents"
  # randomly add documents to both the categories and knowledge sets

  my $i = 0;
  foreach my $entry (@{$self->UI->GetAllEntries}) {
#     if (! ($i % 10)) {
#       print $i."\n";
#     }
    if (! ($i % 100)) {
      print ".";
    }
    ++$i;
    foreach my $class (keys %{$self->UI->Properties->{$entry}}) {
      push @categories, $self->Categories->{$class};
    }
    if (@categories) {
      my $name = $self->UI->Contents->{$entry}->{ID};
      my $content = $self->UI->Contents->{$entry}->{$UNIVERSAL::corpus->UI->ContentField};
      my $d = AI::Categorizer::Document->new
	(name => $name,
	 content => $content,
	 categories => \@categories);
      foreach my $category (@categories) {
	$category->add_document($d);
      }
      push @{$self->Documents}, $d;
    }
  }
  print "\n";
  $self->KnowledgeSet
    (AI::Categorizer::KnowledgeSet->new
     (categories => [values %{$self->Categories}],
      documents => $self->Documents));
  my $classifier = $self->Classifier;
#   $self->Learner
#     (AI::Categorizer::Learner::SVM->new());

  $self->Learner
    (AI::Categorizer::Learner::NaiveBayes->new());

  $self->Learner->train
    (knowledge_set => $self->KnowledgeSet);
  $self->Built(1);
}

sub ClassifyEntry {
  my ($self,%args) = @_;
  if (! defined $self->Built) {
    $self->RebuildIndex;
  }
  my $entry = $args{Entry};
  my $d = AI::Categorizer::Document->new
    (name => $self->UI->Contents->{$entry}->{ID},
     content => $self->UI->Contents->{$entry}->{$UNIVERSAL::corpus->UI->ContentField});
  my $hypothesis = $self->Learner->categorize($d);
  my @matches = $hypothesis->categories;
  my @results = $hypothesis->scores(@matches);
  my $h = {};
  while (@matches) {
    my $category = shift @matches;
    shift @results;
    my $score = shift @results;
    $h->{$category} = $score;
  }
  return $h;
}

1;
