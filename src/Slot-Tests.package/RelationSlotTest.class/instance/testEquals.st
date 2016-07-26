tests
testEquals

	| s1 s2 |
	
	s1 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	s2 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	
	self assert: s1 = s2.
	self assert: s1 hash = s2 hash.