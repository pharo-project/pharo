client list accessing
scrollToShow: aRectangle
	"Scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space."
	
	|offset|
	offset := self scroller offset.
	((aRectangle top - offset y) >= 0 and: [
		(aRectangle bottom - offset y) <= self innerBounds height])
		ifFalse: [offset := offset x @ (
					(aRectangle top min: self scrollTarget height - self innerBounds height))].
	((aRectangle left - offset x) >= 0 and: [
		(aRectangle right - offset x) <= self innerBounds width])
		ifFalse: [offset := (aRectangle left min: self scrollTarget width - self innerBounds width) @ offset y].
	offset = self scroller offset ifFalse: [
		self scroller offset: offset.
		self setScrollDeltas]