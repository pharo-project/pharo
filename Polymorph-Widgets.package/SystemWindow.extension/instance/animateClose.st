*Polymorph-Widgets
animateClose
	"Animate closing."
	
	| smallRect restoredRect rects steps|
	self isMinimized ifTrue: [^self].
	restoredRect := self bounds.
	smallRect := restoredRect scaledAndCenteredIn: (0@0 extent: 20@20).
	smallRect := smallRect align: smallRect center with: restoredRect center.
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		smallRect interpolateTo: restoredRect at: (20 raisedTo: x) - 1 / 19].
	World displayWorldSafely.
	self fastAnimateRectangles: rects