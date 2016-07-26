geometry testing
fullContainsPoint: aPoint
	(self fullBounds containsPoint: aPoint)
		ifFalse: [ ^ false ].	"quick elimination"
	(self containsPoint: aPoint)
		ifTrue: [ ^ true ].	"quick acceptance"
	^ submorphs anySatisfy: [ :m | m fullContainsPoint: aPoint ]