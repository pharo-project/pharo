printing
printTextOn: aStream upTo: endPosition 

	self span == endPosition span ifTrue: [ 
		^self span asStringOn: aStream startingAt: self spanPosition upTo: endPosition spanPosition].

	self span asStringOn: aStream startingAt: self spanPosition.
	self span next printTextOn: aStream upTo: endPosition span.
	endPosition span asStringOn: aStream upTo: endPosition spanPosition.

	