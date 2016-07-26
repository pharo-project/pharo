tests
testDefinitionWithClass

	| s1 s2 |
	
	s1 := #people => ToManyRelationSlot inverse: #x inClass: #SlotExamplePerson.
	s2 := #people => ToManyRelationSlot inverse: #x inClass: SlotExamplePerson.
	
	self assert: s1 = s2.
	self assert: s1 hasInverse 
