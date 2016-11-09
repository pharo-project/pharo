basic-colors
scrollbarColorFor: aScrollbar
	"Answer the colour for the given scrollbar."

	^self settings standardColorsOnly
		ifTrue: [self settings scrollbarColor]
		ifFalse: [aScrollbar lastPaneColor
					 ifNil: [Color white]]