*Polymorph-Widgets
animateMinimize
	"Animate minimizing."
	
	|tb buttonRect restoredRect rects steps|
	tb := self worldTaskbar ifNil: [^self].
	buttonRect := ((tb taskButtonOf: self) ifNil: [^self]) bounds.
	restoredRect := self isFlexed
		ifTrue: [(owner transform
					globalPointToLocal: fullFrame topLeft)
					extent: fullFrame extent]
		ifFalse: [fullFrame].
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		buttonRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects