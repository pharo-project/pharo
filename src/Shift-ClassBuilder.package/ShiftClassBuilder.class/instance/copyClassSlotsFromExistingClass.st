copying
copyClassSlotsFromExistingClass
	"Ugly helper method: I copy over the class slots of the class in my builder environment."

	| anOldClass oldSlots |
	anOldClass := self environment at: name ifAbsent: [ ^ self ].

	oldSlots := anOldClass class classLayout slotScope visibleSlots.
	self classSlots:(oldSlots collect: [ :e | e copy index: nil; yourself ])