testing
needsHorizontalScrolling
	"Answer true, if text layout is wider than view , and therefore could use scrolling"
	^ (offset x > 0) or: [ self width < layout width ]