tests
testDefinitionWithoutInverse

	| slot |
	
	slot := #people => ToManyRelationSlot inClass: #SlotExamplePerson.
	self deny: slot hasInverse