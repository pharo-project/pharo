accessing
mouseDownHighlightColor
	"Answer a good color to use for drawing the mouse down highlight.
	Used the line color if not transparent, otherwise a contrasting color in the
	same way as the line color is determined.
	Fall back to black if all my owners are transparent."

	|c colored |
	c := self lineColor.
	c isTransparent ifFalse: [^c].
	colored := self color isTransparent
		ifTrue: [self firstOwnerSuchThat: [:o | o isWorldOrHandMorph not and: [o color isTransparent not]]]
		ifFalse: [self].
	colored ifNil: [^Color black].
	^colored color luminance > 0.5
		ifTrue: [Color black]
		ifFalse: [Color white]
