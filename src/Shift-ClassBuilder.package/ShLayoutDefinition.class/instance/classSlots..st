accessing
classSlots: anObject
	classSlots := anObject collect: [ :e | e asSlot ].