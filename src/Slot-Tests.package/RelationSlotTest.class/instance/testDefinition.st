tests
testDefinition

	| s1 s2 |
	
	s1 := ToManyRelationSlot named: #people inverse: #x inClass: #SlotExamplePerson.
	s2 := #people => ToManyRelationSlot inverse: #x inClass: #SlotExamplePerson.
	
	self assert: s1 = s2.
	self assert: s1 hasInverse 
