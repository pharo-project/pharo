slider-scrolling
moveLeft: left "Integer - pixels"
	"moves scrollbar inside parent to the specified left distance in pixels and notifies all listeners about that"
	"Highly optimised to not update layout during moving"
	| right |
	right := left + self width.
	self changedWith: [ :b |
		b translateXTo: left.
		b leftPosition: left ].
	
	pagerModel notifyScrollerBoundsChanged: left@right