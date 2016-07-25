drawing
drawLineToggleToTextOn: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge.
	Only draw now if no toggle."

	| myBounds myCenter hLineY hLineLeft myTheme|
	self isSoleItem ifTrue: [ ^self ].
	self hasToggle ifTrue: [^self].
	myBounds := self toggleBounds.
	myCenter := myBounds center.
	hLineY := myCenter y - 1.
	hLineLeft := myCenter x.
	"Draw line from toggle to text. Use optimised form since vertical."
	myTheme := self theme.
	aCanvas
		frameRectangle: (hLineLeft @ hLineY corner: myBounds right + 3 @ (hLineY + 1))
		width: myTheme treeLineWidth
		colors: (myTheme treeLineColorsFrom: lineColor)
		dashes: myTheme treeLineDashes