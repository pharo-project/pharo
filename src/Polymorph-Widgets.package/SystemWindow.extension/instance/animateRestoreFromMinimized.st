*Polymorph-Widgets
animateRestoreFromMinimized
	"Animate restoring from minimised."
	
	|tb buttonRect restoredRect rects steps|
	tb := self worldTaskbar ifNil: [^self].
	buttonRect := ((tb taskButtonOf: self) ifNil: [^self]) bounds.
	restoredRect := self isFlexed
		ifTrue: [(owner transform
					globalPointToLocal: fullFrame topLeft)
					extent: fullFrame extent]
		ifFalse: [fullFrame].
	steps := self theme settings animationSettings numberOfSteps.
	rects := (1/steps to: 1 by: 1/steps) collect: [:x |
		buttonRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects