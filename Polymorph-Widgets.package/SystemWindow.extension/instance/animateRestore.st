*Polymorph-Widgets
animateRestore
	"Animate restoring from maximised."
	
	|expandedRect restoredRect rects steps|
	expandedRect := self bounds.
	restoredRect := self unexpandedFrame.
	steps := self theme settings animationSettings numberOfSteps.
	rects := ((steps - 1)/steps to: 0 by: -1/steps) collect: [:x |
		restoredRect interpolateTo: expandedRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects