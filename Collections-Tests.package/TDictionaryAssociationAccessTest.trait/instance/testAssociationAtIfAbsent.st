tests - dictionary assocition access
testAssociationAtIfAbsent

	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn ifAbsent: [888]. 

	self assert: (result key) = keyIn.
	self assert: (result value ) = (collection at: keyIn ).

	self assert: (collection associationAt: self keyNotIn  ifAbsent: [888] ) = 888