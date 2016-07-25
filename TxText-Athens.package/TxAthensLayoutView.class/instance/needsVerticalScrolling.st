testing
needsVerticalScrolling
	"Answer true if, vertically, text are not fitting completely inside a view, and may use vertical scrolling.
	
	This is simple: if it fits completely, layout start = text start, layout end = text end
	"
	
	^ (layout startPosition isAtStart and: [ layout endPosition isAtEnd]) not