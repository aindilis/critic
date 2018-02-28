decideTruthValue(likes(Agent,Z)) :-
	likes(Agent,X),
	hasProperty(X,P1),
	likes(Agent,Y),
	hasProperty(Y,P2),
	hasProperty(Z,P1),
	hasProperty(Z,P2).

%% write something that takes any assertion containing something and
%% turns it into a property formulation for that, if indeed that makes sense

%% hasProperty(Thing,Property) :-
%% 	allTermAssertions(Thing,Assertions),
%% 	member(Assertion,Assertions),
%% 	rewriteAssertionMentioningThingIntoProperty(Thing,Assertion,Property).

%% rewriteAssertionMentioningThingIntoProperty(Thing,Assertion,Property) :-
%% 	true.