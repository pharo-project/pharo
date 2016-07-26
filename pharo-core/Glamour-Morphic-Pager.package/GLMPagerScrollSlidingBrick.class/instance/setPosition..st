slider-draging
setPosition: aPoint
	"aPoint x - is my bounds left
	aPoint y - is my bounds right"
	"highly optimised to not update layout and do everything very clean"
	self changedWith: [ :b | b
			translateXTo: aPoint x;
			leftPosition: aPoint x;
			doNotLayoutDuring: [
				b width: aPoint y - aPoint x.
				b wrappedBounds markWidthClean ] ].
	
	self pagerModel notifyScrollerBoundsChanged: aPoint