#!/usr/bin/perl -w

# integrate Critic's existing GUI

# this is the GUI for Critic

# have a feature that you can type in a goal or something and ask for
# all instances that either entail it or are entailed by it

# you can also classify or cluster it with the other entries

# have the ability to add class tags and to edit the classes, even
# have a class ontology, as borrowed from some other ontologies


# have the ability to rate entries, both with a scalar and with text

# have the ability to assert knowledge about entries, and have buttons
# for common assertions to make

# have the ability to formalize the entry, etc

# use the appropriate sources


my $critiques =
  [
   "plans",
   "recipes",
   "software",
  ];

my $selections =
  [
   "by property",
   "by entailment",
  ];

my $capabilities =
  [
   "partitioning",
   "verify groups",
  ];

my $sources =
  [
   "CSO",
   "Gourmet",
   "UniLang",
   "PSE",
   "CPAN",
   "Apt",
   "Barcode databases",
   "company information",
   "Clear/Digilib reading lists",
   "RSS feeds",
   "Audience messages",
  ];

# tools AI::Categorizer, also use our advanced rival categorizer

# critic explore versus exploit trade off

# use heterogenous, semistructured extraction tools for critic, create
# wrappers for various sources, like rdf dump, xml file, database
# output, etc.

# use critic, or whatever, to match a given entry to its normalized form

# use critic to rate words as being important, and use this
# information, as well as others, to help digest find relevant
# application data on the web

# use critic to enable classification of data'


# enable the software-finder approach
# should add a feature to critic where it looks at sources deemed similar to highly rated ones first

# got the beginning of disciple and improve critic dramatically.
# Critic now seems to be geniunely working, although we should add the
# ability to infer from time spent looking at an item what the user\'s
# interest is, put an interval of expected rating, distinguish between
# the internal rating and the users rating, maybe call it utility, and
# add time-dependent features to the analysis.  Also look at
# carbonell\'s javelin algorithm as a metric

# yes recommendation systems are key to this whole approach

# basically critic is like a knowledge base editor with some machine
# learning features to suggest knowledge by analogy that may be
# useful, and basically to help the user alter the state of relations
# pertaining to a particular system

# critic has some of the beginnings of a source recommendation system,
# and a reputation management system.  social knowledge is critical to
# advancing our cause

# add the ability to cryptographically sign knowledge, etc


# critic should be able to compare multiple sources at once, for
# interrelation, e.g. perl modules with frdcsa subsystems

# undo

# Modules: Weka

# have a slide for exploration versus exploitation

# add pse functions as well

# Rating Annotation

# Dataset Manager

my $buttons =
  [
   "Classify",
   "Cluster",
   "Formalize",
   "Generate Topical Ratings",
  ];


my $emacscriticfunctions =
  [
   "critic-mode",
   "critic-next",
   "critic-previous",
   "critic-get-entry",
   "critic-search",
   "critic-rate",
   "critic-knowledge-editor",
   "critic-ke-assert",
   "critic-ke-edit",
   "critic-ke-unassert",
   "critic-relate",
   "critic-compare",
   "critic-reload",
   "critic-resort",
   "critic-select",
   "critic-deselect",
   "critic-select-all",
   "critic-deselect-all",
   "critic-select-region",
   "critic-deselect-region",
   "critic-push-up",
   "critic-push-down",
   "critic-view",
   "critic-classify",
   "non-nil",
   "critic-unilang-view-recent-entries",
   "critic-unilang-view-recent-goals",
   "critic-critique-entries",
  ];

my $emacspsefunctions =
  [
   "pse-assert-property-about-goal",
   "pse-get-item",
   "pse-completed",
   "pse-incomplete",
   "pse-query-completed",
   "pse-comment",
   "pse-solution",
   "pse-display-entry-for-id-at-point",
   "pse-get-entry-for-id",
   "pse-belongs-to-system",
   "pse-find-similar-goals-to-goal-at-point",
   "pse-typical-rejected",
   "pse-typical-skipped",
   "pse-typical-ridiculous",
   "pse-typical-obsolete",
   "pse-quick-goal",
   "pse-quick-goal-completed",
   "pse-add-entry-get-entryid",
   "pse-quick-precondition-for-eap",
   "pse-quick-depends-on-eap",
   "pse-quick-eases-eap",
   "pse-quick-interentry-formula",
   "pse-add-person",
   "pse-get-person",
   "pse-assigned-to",
   "pse-assigned-by",
   "pse-search-goals",
   "pse-extension-do-action-for-goal-at-point",
   "pse-review-habitual",
   "pse-visualize-goals",
  ];

