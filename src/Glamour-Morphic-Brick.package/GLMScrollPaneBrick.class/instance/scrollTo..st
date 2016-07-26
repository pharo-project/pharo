scrollpane-scrolling
scrollTo: aBrick
	|delta|
	"if already visible no need to scroll"

	(self isVisible: aBrick) ifTrue: [ ^ self ].
	
	delta := ((self allowedBounds top - band globalBounds top) - (aBrick globalBounds top - band globalBounds top)).
	delta < 0 ifTrue: [ delta := delta + self allowedBounds height - aBrick height ].
		
	self band translateY: delta.
	self isBottomOverscrolled ifTrue: [ self moveBandBottom ].
	self isTopOverscrolled ifTrue: [ self moveBandTop ].
	
	self update.
	self changed.
	
	