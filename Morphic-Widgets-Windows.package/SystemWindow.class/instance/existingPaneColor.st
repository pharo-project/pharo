panes
existingPaneColor
	"Answer the existing pane color for the window, obtaining it from the first paneMorph if any, and fall back on using the second stripe color if necessary."

	| aColor |
	aColor := self valueOfProperty: #paneColor.
	aColor ifNil: [self setProperty: #paneColor toValue: (aColor := self paneColor)].
	^aColor.