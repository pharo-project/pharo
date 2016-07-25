drawing
drawLinesToNextSiblingOn: aCanvas hasToggle: hasToggle
	"Draw line from me to next sibling"
	
	| myBounds nextSibBounds vLineX myCenter vLineTop vLineBottom myTheme ldelta gap |
	myBounds := self toggleRectangle.
	nextSibBounds := self nextSibling toggleRectangle.
	myCenter := myBounds center.
	vLineX := myCenter x.
	gap := (container notExpandedForm extent y // 2) + 1.
	vLineTop := myCenter y + (self hasToggle ifTrue: [gap] ifFalse: [0]).
	vLineBottom := nextSibBounds center y - (self nextSibling hasToggle ifTrue: [gap] ifFalse: [0]).
	"Draw line from me to next sibling"
	myTheme := self theme.
	ldelta := container treeLineWidth // 2.
	aCanvas
		frameRectangle: (vLineX - ldelta @ vLineTop corner: vLineX + ldelta + (container treeLineWidth \\ 2) @ vLineBottom)
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: self lineColor)
		dashes: self treeLineDashes