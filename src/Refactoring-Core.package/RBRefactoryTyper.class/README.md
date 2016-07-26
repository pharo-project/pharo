I am a type guesser used by refactoring operations.

I try to determine the types used for variables in a class.

Refactoring operations are using me to provide a possible list of classes that are affected by a refactoring. 
For example, for moving or delegating a method implementation to an instance variable, it might be necessary to add this implementation to multiple classes.

I analyze message sends to the instance variables and try to guess the type by looking at implementors of that messages. 
(If a method sends ifTrue:ifFalse: to an instance variable I can guess that the instance variable may be a Boolean object).

If I can guess the type to be a collection, I'll try to investigate the types of the possible contained elements as well.
You can ask for the guessed type of the variable: 
typer guessTypeFor: 'variablename'
and the contained elements: 
typer guessTypeFor: '-variablename-'

Example usage: 
| typer |
"create and initialize for a class to analyze"
typer := (RBRefactoryTyper new runOn: RBMessageNode).

"guess types for RBMessageNodes instane var 'arguments' "
typer  guessTypesFor:'arguments'.  "a Set(SequenceableCollection)"

"guess types for objects that may be put  into  'arguments' collection "
typer  guessTypesFor:'-arguments-' "a Set(RBBlockNode RBMethodNode)"


You can print a full report on all results with #printString.

(RBRefactoryTyper new runOn: Point ) printString.
"'Point
	x	<Integer>
	y	<Integer>
'"

There are two class side methods, one for creating an instance and setting the environment used to resolve names - a RBNamespace.

And another one for guessing types in a parseTree:

RBRefactoryTyper typesFor: 'var' in: (RBParser parseExpression: 'var squared') model: RBNamespace new "a Set(Number Collection)"
(Here, I can guess the types Number and Collections as these are the classes implementing the message #squared)

