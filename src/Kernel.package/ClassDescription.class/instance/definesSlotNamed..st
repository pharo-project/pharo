slots
definesSlotNamed: aString
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self slotNames includes: aString