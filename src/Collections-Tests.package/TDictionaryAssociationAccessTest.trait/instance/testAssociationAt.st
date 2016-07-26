tests - dictionary assocition access
testAssociationAt

	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn. 

	self assert: (result key) = keyIn.
	self assert: (result value ) = (collection at: keyIn ).