geometry
extent: newExtent
	"Update the receiver's extent. Hide/show the scrollbars and resize the scroller
	as necessary."

	"we need to compare against 1 pixel, otherwise may end up in an infinite loop.
	If newExtent, current extent and current position represent a float number
	inner scrollpane rounding gives difference around 0.5 pixel disallowing us
	to use Point>>#closeTo:. It is quite dirty fix, general one would require enormous
	amount of work to improve how morphic works with floats. This issue successfully
	addressed in Bloc"
	(bounds extent - newExtent) abs < (1@1)
		ifTrue: [ ^ self ].
	super extent: newExtent.
	self owner ifNotNil: [ :o | o scrollerExtentChanged ].
	self wrapped
		ifTrue: [ self updateScrollbars ]