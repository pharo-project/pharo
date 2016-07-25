scrollbars
hScrollbarInterval
	"Answer the computed size of the thumb of the horizontal scrollbar."
	
	^self scrollBounds width asFloat / self scrollTarget width min: 1.0.