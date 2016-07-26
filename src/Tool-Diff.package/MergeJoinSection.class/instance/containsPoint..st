as yet unclassified
containsPoint: aPoint
	"Answer whether the receiver contains the given point."

	^(super containsPoint: aPoint) or: [
		self stateIcon notNil and: [self stateIconBounds containsPoint: aPoint]]