geometry
vLeftoverScrollRange
	"Return the entire scrolling range minus the currently viewed area."
	
	^self scrollTarget height - self scrollBounds height max: 0
