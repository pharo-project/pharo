slots
hasSlotNamed: aString
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self allSlots anySatisfy: [:slot | slot name = aString]