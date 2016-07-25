operations
rightButtonEnabled
	"Answer whether the button for scrolling right should be enabled."

	^self wrapScrolling or: [self visibleTabRange last < self tabs size]