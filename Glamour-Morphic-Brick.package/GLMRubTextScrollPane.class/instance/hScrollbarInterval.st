scrollbars
hScrollbarInterval
	"Answer the computed size of the thumb of the horizontal scrollbar."
	self scrollTarget width = 0 ifTrue: [ ^ 0 ].
	^ self scrollBounds width asFloat / self scrollTarget width min: 1.0.