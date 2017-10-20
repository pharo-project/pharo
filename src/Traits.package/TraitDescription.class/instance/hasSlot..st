slots
hasSlot: aSlot
	"Return true whether the receivers hierarchy defines an instance variable named aString."
	
	^ self allSlots identityIncludes: aSlot