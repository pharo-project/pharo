drawing
drawLinesToNextSiblingOn: aCanvas lineColor: lineColor hasToggle: hasToggle
	"Draw line from me to next sibling"
	
	| myBounds nextSibBounds vLineX myCenter vLineTop vLineBottom myTheme|
	myBounds := self toggleBounds.
	nextSibBounds := self nextSibling toggleBounds.
	myCenter := myBounds center.
	vLineX := myCenter x.
	vLineTop := myCenter y + 1.
	vLineBottom := nextSibBounds center y - 1.
	"Draw line from me to next sibling"
	myTheme := self theme.
	aCanvas
		frameRectangle: (vLineX @ vLineTop corner: vLineX + 1 @ vLineBottom)
		width: myTheme treeLineWidth
		colors: (myTheme treeLineColorsFrom: lineColor)
		dashes: myTheme treeLineDashes