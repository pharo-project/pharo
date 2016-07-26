I am a cluster for objects that have to be replaced in the object graph by another one (at serialization time).

Examples of use:

1) Suppose you want to substitute instances of WriteStream by nil. In such case, WriteStream has to implement this method:

fuelAccept: aVisitor
	^aVisitor visitSubstitution: self by: nil


2) Suppose you want to substitute every integer in the graph by its string representation. In such case you should configure the analyzer through:

anAnalyzer when: [:x | x isInteger] substituteBy: [:x | x printString].

In this way, when you serialize #(1 2), you will then materialize #('1' '2')