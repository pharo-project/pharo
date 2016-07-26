tests - iterate on sequenced reable collections
testCollectFromTo
	
	| result |
	result:=self nonEmptyMoreThan1Element 
		collect: [ :each | each ]
		from: 1
		to: (self nonEmptyMoreThan1Element size - 1).
		
	1 to: result size
		do: [ :i | self assert: (self nonEmptyMoreThan1Element at: i) = (result at: i) ].
	self assert: result size = (self nonEmptyMoreThan1Element size - 1)