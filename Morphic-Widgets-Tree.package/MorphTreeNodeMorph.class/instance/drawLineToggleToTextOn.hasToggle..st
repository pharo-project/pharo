drawing
drawLineToggleToTextOn: aCanvas hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge.
	Only draw now if no toggle."

	| myBounds myCenter hLineY hLineLeft myTheme ldelta |
	self isSoleItem ifTrue: [ ^self ].
	self hasToggle ifTrue: [^self].
	myBounds := self toggleRectangle.
	myCenter := myBounds center.
	hLineY := myCenter y - 1.
	ldelta := container treeLineWidth // 2.
	hLineLeft := myCenter x - ldelta.
	"Draw line from toggle to text. Use optimised form since vertical."
	myTheme := self theme.
	aCanvas
		frameRectangle: (hLineLeft @ (hLineY ) corner: myBounds right + 3  + ldelta @ (hLineY + container treeLineWidth ))
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: (self parent ifNil: [self lineColor] ifNotNil: [self parent lineColor]))
		dashes: self treeLineDashes