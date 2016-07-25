accessing
copyClassSlotsFromExistingClass
	"Ugly helper method: I copy over the class slots of the class in my builder environment."
	| oldClass |
	oldClass := 
		self environment 
			at: name 
			ifAbsent: [ ^ self ].

	self classSlots: oldClass class classLayout slotScope visibleSlotNames.