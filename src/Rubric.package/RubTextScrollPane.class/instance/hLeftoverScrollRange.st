scrollbar managing
hLeftoverScrollRange
	"Return the entire scrolling range minus the currently viewed area."
	^self textArea width  - self scrollBounds width max: 0