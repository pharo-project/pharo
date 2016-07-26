validation
checkInheritedSlots
	
	self host superclass 
		ifNil: [ ^ self ].
	self host classLayout
		ifNil: [ ^ self ].
		
	self 
		assert: (self host superclass classLayout size <= self host classLayout size)
		description: [ (self host name, ' does not inherit all slots from ', self host superclass name) ].
	
	"check that the we have the same slots at the same indices as in the superclass"
	self host superclass classLayout allSlots withIndexDo: [ :slot :index|
		| localSlot |
		localSlot := (self host classLayout allSlots at: index).
		self 
			assert: slot = localSlot
			description: [ 'Slot "', localSlot name, '" index at position ', index asString, ' conflicts with slot "', slot name, '" in ', self host superclass asString ]].