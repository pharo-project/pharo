*Polymorph-Widgets
animateMaximize
	"Animate maximizing from restored."
	
	|expandedRect restoredRect rects steps|
	expandedRect := self fullscreenBounds.
	restoredRect := self bounds.
	steps := self theme settings animationSettings numberOfSteps.
	rects := (1/steps to: 1 by: 1/steps) collect: [:x |
		restoredRect interpolateTo: expandedRect at: (20 raisedTo: x) - 1 / 19].
	self fastAnimateRectangles: rects