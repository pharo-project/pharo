deferring
deferUpdatesIn: aRectangle while: aBlock
	| result |
	(self class deferUpdates: true) ifTrue: [^aBlock value].
	result := aBlock value.
	self class deferUpdates: false.
	self forceToScreen: aRectangle.
	^result