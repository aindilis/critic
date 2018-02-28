package UniLang::Util::Corpus::Corpus;

use KBFS::Cache;
use KBFS::Cache::Item;

use String::Random;
use XML::Twig;

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker new_with_init => 'new',
  get_set => [ qw / CorpusDir Cache XMLDoc Documents / ];

sub init {
  my ($self,%args) = (shift,@_);
  $self->CorpusDir($args{CorpusDir});
  $self->XMLDoc(XML::Twig->new);
  $self->Cache(KBFS::Cache->new(CacheDir => $self->CorpusDir));
  $self->Documents(());
}

sub Gather {
  my ($self,%args) = (shift,@_);
  my @docs = ();
  foreach my $result (split /\n/, `locate -i thoughts`) {
    if ($result =~ /\/thoughts$/i) {
      push @docs, KBFS::Cache::Item->new(URI => $result)
	unless $self->Cache->Contains($result);
    }
  }
  foreach my $result (split /\n/, `locate -i todo`) {
    if ($result =~ /\/todo$/) {
      push @docs, KBFS::Cache::Item->new(URI => $result)
	unless $self->Cache->Contains($result);
    }
  }
  $self->Documents(\@docs);
}

sub Screen {
  my ($self,%args) = (shift,@_);
  my @classified = ();
  foreach my $doc (@{$self->Documents}) {
    my $result = $doc->URI;
    print "\n\n". "#" x 80 ."\n";
    print "<$result>\n";
    print "#" x 80 ."\n";
    print $doc->HeadOfContents;
    print "#" x 80 ."\n";
    if ($self->Approve()) {
      push @classified, $doc;
    }
  }
  $self->Documents(@classified);
  foreach my $doc (@{$self->Documents}) {
    $self->Cache->Cache(Item => $doc);
  }
}

sub Approve {
  my ($self,%args) = (shift,@_);
  print "Is this correct? ([yY]|[nN])\n";
  my $antwort = <>;
  if ($antwort =~ /^[yY]$/) {
    return 1;
  }
  return 0;
}

#sub parse {
#  my $twig = XML::Twig->new(pretty_print => 'indented');
#  foreach my $file ($self->Cache->Contents) {
#    # read in file to string, then parse
#    my $string = `cat $corpusdir/$file`;
#    parse_string($string);
#  }
#}
#
#sub Verify {
#  my $string = shift;
#  print "#" x 80 . "\n$string\n" . "#" x 80 . "\n";
#  return approve;
#}
#
#sub old_parse_string {
#  my $string = shift;
#  #foreach my $line (split /\n/,$string) { 
#  %patterns = ("(\([^\)]*\))" => 1,
#	       "(\([^\)]*\))" => 1);
#
#  while ($string) {
#    # match what is believed to be  the next goal, and prompt the user
#    # to verify
#    @patterns = keys %patterns;
#    $answer = "";
#    while (!$answer && @patterns) {
#      $pattern = pop @patterns;
#      $string =~ /$pattern/s;
#      $answer = ${$patterns{$pattern}};
#      if ($self->Verify($answer)) {
#	$string =~ s/$pattern//;
#	print "<line>\n\t$answer\n</line>\n";
#      } else {
#	$answer = "";
#      }
#    }
#  }
#}
#
## so parsing  doesn't entirely work yet,  but we still  get some valid
## samples that we can use to begin working with the planning system.
#
#sub parse_string {
#  my $string = shift;
#  foreach $line ($string =~ /(\([^\)]*\))/sg) {
#    print "<id>
#<contents>\n\t$line\n</contents>\n";
#  }
#}
#
#sub Generate {
#  my ($self, %args) = (shift,@_);
#  # prints in XML form, i.e.
#  my $twig = XML::Twig->new(pretty_print => 'indented');
#  my $message = XML::Twig::Elt->new('message');
#  my $id = XML::Twig::Elt->new('id');
#  $id->set_text(1);
#  $id->paste('last_child', $message);
#
#  my $sender = XML::Twig::Elt->new('sender');
#  $sender->set_text($self->Sender);
#  $sender->paste('last_child', $message);
#
#  my $receiver = XML::Twig::Elt->new('receiver');
#  $receiver->set_text("UniLang");
#  $receiver->paste('last_child', $message);
#
#  my $date = XML::Twig::Elt->new('date');
#  $date->set_text($self->Date);
#  $date->paste('last_child', $message);
#
#  my $contents = XML::Twig::Elt->new('contents');
#  $contents->set_text($self->Contents);
#  $contents->paste('last_child', $message);
#
#  $message->sprint;
#}

1;
