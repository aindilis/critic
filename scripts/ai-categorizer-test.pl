#!/usr/bin/perl -w

use AI::Categorizer;
use Data::Dumper;
use DBI;

my $dbh = DBI->connect
  ("DBI:mysql:database=" .
   "unilang".
   ";host=localhost",
   "root", "",
   {
    'RaiseError' => 1});

sub MySQLDo {
  my (%args) = @_;
  my $statement = $args{Statement};
  if ($statement =~ /^select/i) {
    $sth = $dbh->prepare($statement);
    $sth->execute();
    return $sth->fetchall_hashref($args{KeyField});
  } else {
    $dbh->do($statement);
  }
}

my $c = new AI::Categorizer();

# load results from database for use in learning
my $results = MySQLDo
  (Statement => "select ID,Contents from messages where Sender='UniLang-Client'",
   KeyField => "ID");
my $l;

# create categories
my @categorynames = qw(A B C D E F G H I J K);
my @categories;
foreach my $categoryname (@categorynames) {
  push @categories, AI::Categorizer::Category->by_name(name => $categoryname);
}

# load "documents"
# randomly add documents to both the categories and knowledge sets
my @documents;
my @test;
my @train;
my $i = 0;
foreach my $key (keys %$results) {
  if (! ($i % 100)) {
    print $i."\n";
  }
  ++$i;
  push @documents, $d;
  if (int(rand(10)) > 7) {
    my $d = AI::Categorizer::Document->new
      (name => $results->{$key}->{ID},
       content => $results->{$key}->{Contents});
    push @test, $d;
  } else {
    # add $d to a random category
    my $category = $categories[int(rand(scalar @categories))];
    my $d = AI::Categorizer::Document->new
      (name => $results->{$key}->{ID},
       content => $results->{$key}->{Contents},
       categories => [$category]);
    $category->add_document($d);
    push @train, $d;
  }
}

# create a knowledge set
my $k = new AI::Categorizer::KnowledgeSet
  (categories => \@categories,
   documents => \@train);
my $nb = new AI::Categorizer::Learner::NaiveBayes();
$nb->train(knowledge_set => $k);

foreach my $d (@test) {
  # After training, use the Learner for categorization
  my $hypothesis = $nb->categorize($d);
  # print Dumper($hypothesis);
  print "Best assigned category: ", $hypothesis->best_category, "\n";
  print "All assigned categories: ", join(', ', $hypothesis->categories), "\n";
}
