package Critic::Review;

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Item ID Start Finish Duration Value / ];

sub init {
  my ($self, %args) = (shift,@_);
  $self->ID($args{ID});
  $self->Start($args{Start});
  $self->Finish($args{Finish});
  $self->Duration($args{Duration});
  $self->Value($args{Value});
}

1;
