scrollbars
fitScrollTarget
	"If the scroller is bigger than the scroll target then
	resize the scroll target to fill the scroller."
	
	|extra|
	extra := 0.
	self scroller width > self scrollTarget width
		ifTrue: [self scrollTarget width: self scroller width]
		ifFalse: [extra := self scrollBarThickness].
	self scroller height - extra > self scrollTarget height
		ifTrue: [self scrollTarget height: self scroller height + extra]