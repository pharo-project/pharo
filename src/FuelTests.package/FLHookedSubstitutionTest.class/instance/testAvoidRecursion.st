tests
testAvoidRecursion

	| aClass result original | 
	original := FLClassWithRecursiveSubstitution new index: 1.
	
	result := self resultOfSerializeAndMaterialize: original.
	
	self assert: result index = 2