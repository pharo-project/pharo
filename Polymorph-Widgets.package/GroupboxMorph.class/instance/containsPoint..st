geometry testing
containsPoint: aPoint
	"Override here to check the label and content instead."
	
	^(super containsPoint: aPoint) and: [
		(self labelMorph containsPoint: aPoint) or: [
		self contentMorph containsPoint: aPoint]]