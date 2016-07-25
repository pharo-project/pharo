*GT-Playground
matchesOnStream: aStream subexpressionsDo: aBlock
	| subexpressions |
	[ self searchStream: aStream ] whileTrue: [ 
		subexpressions := ((1 to: self subexpressionCount) collect: [ :each | (self subexpression: each) ]).
		aBlock value: subexpressions first value: subexpressions allButFirst ]