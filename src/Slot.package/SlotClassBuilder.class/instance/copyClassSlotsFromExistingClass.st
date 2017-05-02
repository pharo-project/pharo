accessing
copyClassSlotsFromExistingClass
	"Ugly helper method: I copy over the class slots of the class in my builder environment."

	| oldClass oldSlots |
	oldClass := self environment at: name ifAbsent: [ ^ self ].

	oldSlots := oldClass class classLayout slotScope visibleSlots.
	self classSlots:(oldSlots collect: [ :e | e copy index: nil; yourself ])