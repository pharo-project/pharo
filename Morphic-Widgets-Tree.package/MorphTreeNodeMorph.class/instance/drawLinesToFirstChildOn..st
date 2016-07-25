drawing
drawLinesToFirstChildOn: aCanvas
	"Draw line from me to first child.
	Don't bother if the first child has a toggle.."

	| vLineX vLineTop vLineBottom childBounds childCenter myTheme ldelta |
	self firstChild hasToggle ifTrue: [^self].
	childBounds := self firstChild toggleRectangle.
	childCenter := childBounds center.
	vLineX := childCenter x.
	vLineTop := bounds bottom.
	ldelta := container treeLineWidth // 2.
	self firstChild hasToggle
		ifTrue: [vLineBottom := childCenter y - (childBounds height // 2) + ldelta]
		ifFalse: [vLineBottom := childCenter y - 2].
	myTheme := self theme.
	aCanvas
		frameRectangle: (vLineX - ldelta @ vLineTop corner: (vLineX + ldelta + (container treeLineWidth \\ 2)) @ vLineBottom)
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: self lineColor)
		dashes: self treeLineDashes