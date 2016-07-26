scrollbars
vScrollbarInterval
	"Answer the computed size of the thumb of the vertical scrollbar."
	
	^self scrollBounds height asFloat / self scrollTarget height min: 1.0.