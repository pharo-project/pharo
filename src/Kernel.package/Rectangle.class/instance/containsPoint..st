testing
containsPoint: aPoint 
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]