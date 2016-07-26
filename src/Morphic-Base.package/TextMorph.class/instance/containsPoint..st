geometry testing
containsPoint: aPoint
	(super containsPoint: aPoint) ifFalse: [^ false].  "Not in my bounds"
	container ifNil: [^ true].  "In bounds of simple text"
	self startingIndex > text size ifTrue:
		["make null text frame visible"
		^ super containsPoint: aPoint].
	"In complex text (non-rect container), test by line bounds"
	^ self paragraph containsPoint: aPoint
