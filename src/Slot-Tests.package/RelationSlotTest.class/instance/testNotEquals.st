tests
testNotEquals

	| s1 s2 |
	
	s1 := ToManyRelationSlot named: #slot inverse: #x inClass: #SlotExamplePerson.
	s2 := ToManyRelationSlot named: #slot inverse: #y inClass: #SlotExamplePerson.
	
	self deny: s1 = s2.
