slots
definesSlot: aSlot
	"Return true whether the receiver defines an instance variable named aString"
	
	^ self slots identityIncludes: aSlot