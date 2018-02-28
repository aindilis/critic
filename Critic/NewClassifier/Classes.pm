package Critic::NewClassifier::Classes;

use PerlLib::Collection;

use GraphViz;
use Math::PartialOrder::Std;
use Math::PartialOrder::Loader;

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';


sub AddClass {

}

sub RemoveClass {

}

sub ChangeClassLocation {

}

sub FixClassesWhichHaveBeenChanged {

}


use Class::MethodMaker new_with_init => 'new',
  get_set => [ qw /  / ];

sub init {
  my ($self,%args) = (shift,@_);
  $self->IndexLocation($args{IndexLocation});
}

sub Cuculainn {
  $h = Math::PartialOrder::Std->new({root=>'whatever'}); # make a new hierarchy

  $h->add('yup');	     # 'yup' is a new child-type of 'whatever'
  $h->add('nope');		# ... and so is 'nope'

  $h->add(qw(maybe yup nope)); # 'maybe' inherits from 'yup' and 'nope'
  $h->add(qw(maybenot yup nope)); # ... and so does 'maybenot'

  $h->add(qw(whoknows maybe maybenot));	# 'whoknows' is all of the above

  #
  # Do stuff with it
  #
  @types = $h->types;		# get all the types in the hierarchy
  @yups = $h->descendants('yup'); # ... or just those that are 'yup's
  @kids = $h->children('yup'); # ... or those that are directly 'yup's

  @ancs = $h->ancestors('yup');	# get all ancestors of 'yup'
  @prts = $h->parents('nope');	# ... or just the direct parents

  @sorted = $h->subsort(@types); # sort @types by inheritance

  #
  # Type Operations
  #
  @lubs = $h->lub(qw(maybe maybenot)); # @lubs = ('whoknows')
  @glbs = $h->glb(qw(yup nope)); # @glbs = ('whatever')

  $lub = $h->njoin(qw(maybe maybenot)); # $lub = 'whoknows'
  $lub = $h->njoin(qw(yup nope)); # ... non-CCPO produces a warning

  $glb = $h->nmeet(qw(yup nope)); # $glb = 'whatever'
  $glb = $h->nmeet(qw(maybe maybenot));	# ... non-CCPO produces a warning

  #
  # Persistence
  #
  use Math::PartialOrder::Loader;

  $h->save('h.gt');		# save to text file
  $h->load('h.gt');		# load from text file

  $h->store('h.bin');		# store binary image
  $h->retrieve('h.bin');	# retrieve binary image

  # ... and much, much (too much) more ....
}

  1;
