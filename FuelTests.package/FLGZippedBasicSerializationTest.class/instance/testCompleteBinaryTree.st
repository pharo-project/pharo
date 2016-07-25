tests
testCompleteBinaryTree

	| aCollection numberOfLeafs |
	numberOfLeafs := 2 raisedTo: 13.
	aCollection := OrderedCollection new.
	1 to: numberOfLeafs do: [ :i | 
		aCollection add: (
			FLPair new 
				left: (2 * i); 
				right: (2 * i) + 1;
				yourself) ].
	
	[aCollection size > 1] 
		whileTrue: [
			aCollection := aCollection pairsCollect: [ :leftPair :rightPair | 
				FLPair new 
					left: leftPair;
					right: rightPair;
					yourself ] ].

	self assertSerializationEqualityOf: aCollection