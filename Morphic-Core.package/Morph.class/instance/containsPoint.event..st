events-processing
containsPoint: aPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	(self fullBounds containsPoint: aPoint) ifFalse:[^false].
	(self containsPoint: aPoint) ifTrue:[^true].
	self submorphsDo:[:m|
		(m isLocked and:[m fullContainsPoint: 
			((m transformedFrom: self) globalPointToLocal: aPoint)]) ifTrue:[^true]].
	^false