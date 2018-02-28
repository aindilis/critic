package Critic::Item;

use Critic::Review;
use Data::Dumper qw (Dumper);

use Date::Calc qw ( Today_and_Now Mktime );

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / CID Reviews Start Finish Similarity / ];

sub init {
  my ($self, %args) = (shift,@_);
  $self->CID($args{CID});
  $self->Reviews({});
  $self->Similarity({});
}

sub CurrentTime {
  my ($self, %args) = (shift,@_);
  return Mktime(Today_and_Now());
}

sub CalculateDuration {
  my ($self, %args) = (shift,@_);
  if ($self->Start && $self->Finish) {
    return $self->Finish - $self->Start;
  } else {
    return 0;
  }
}

sub MarkStart {
  my ($self, %args) = (shift,@_);
  $self->Start($self->CurrentTime);
}

sub MarkFinish {
  my ($self, %args) = (shift,@_);
  $self->Finish($self->CurrentTime);
  $self->NewReview
    (Value => $args{Value});
  $self->Start(0);
  $self->Finish(0);
}

sub ReviewCount {
  my ($self,%args) = (shift,@_);
  my $count = 0;
  foreach $review (values %{$self->Reviews}) {
    if ($review->Value =~ /\d+/) {
      ++$count;
    }
  }
  return $count;
}

sub SeenCount {
  my ($self,%args) = (shift,@_);
  return scalar values %{$self->Reviews};
}

sub NewReview {
  my ($self,%args) = (shift,@_);
  my $review = Critic::Review->new
    (ID => $self->SeenCount,
     Start => $self->Start,
     Finish => $self->Finish,
     Duration => $self->CalculateDuration,
     Value => $args{Value});
  print Dumper($review);
  $self->AddReview
    (Review => $review);
}

sub AddReview {
  my ($self,%args) = (shift,@_);
  $self->Reviews->{$args{Review}->ID} = $args{Review};
}

sub ListReviews {
  my ($self,%args) = (shift,@_);
  return values %{$self->Reviews};
}

sub ExpectedAdjustedRating {
  my ($self,%args) = (shift,@_);
  # lets come up with a more accurate rating system.
  # first of all compute the recency that the viewer has seen this
  my $time = $self->CurrentTime;
  my $expect;
  if ($self->ReviewCount || $self->SeenCount) {
    my $count = 0;
    my $total = 0;
    foreach $review ($self->ListReviews) {
      my $diff = $time - $review->Finish;
      if ($review->Value =~ /^\d+$/) {
	$total += $review->Value;
	++$count;
      }
    }
    if ($count) {
      #print Dumper($self->Reviews);
      #print "<total: $total><count: $count><seen: ".$self->SeenCount.">\n";
      $expect = ($total / $count) / ($self->SeenCount + 1);
    } else {
      $expect = $self->ExpectedNewRating;
      #$expect = 0;
    }
  } else {
    $expect = $self->ExpectedNewRating;
  }
  return $expect;
}

sub CIDtoItem {
  my ($self,$cid) = (shift,shift);
  return $UNIVERSAL::critic->Items->{$cid};
}

sub ExpectedNewRating {
  my ($self,%args) = (shift,@_);

  #Look at the original ratings of items that are from the same sources
  #let's compute a similarity matrix
  # from the same site it is  say 25% similar, from the same directory
  # it is %75 similar

  my $avgprior = 2;
  my $rating = 0;
  my $count = 0;
  my $similarity = $self->Similarity;
  foreach $key (keys %{$similarity}) {
    my $cid = $similarity->{$key}->{CID};
    my $item = $self->CIDtoItem($cid);
    my $sim = $similarity->{$key}->{Sim};
    if ($item->ReviewCount && ($item ne $self)) {
      $rating += ($item->ExpectedAdjustedRating * $item->SeenCount * $sim) + ($avgprior * (1-$sim));
      ++$count;
    }
  }
  if ($count) {
    $rating = $rating / $count / ($self->SeenCount + 1);
  } else {
    $rating = $avgprior - 1 + rand(1);
  }
  return $rating;
}

1;
