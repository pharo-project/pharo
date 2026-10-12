Extension { #name : 'FormCanvas' }

{ #category : '*Morphic-Base' }
FormCanvas >> drawString: aString from: firstIndex to: lastIndex autoBoundAt: aPoint font: fontOrNil color: c [

	| font textStyle portRect bounds character width carriageReturn lineWidth lineHeight |
	carriageReturn := Character cr.
	width := lineWidth := 0.
	font := StandardFonts codeFont.
	textStyle := font textStyle.
	lineHeight := textStyle lineGrid.
	1 to: aString size do: [ :i |
			character := aString at: i.
			width := character = carriageReturn
				         ifTrue: [
						         lineWidth := lineWidth max: width.
						         lineHeight := lineHeight + textStyle lineGrid.
						         0 ]
				         ifFalse: [ width + (font widthOf: character) ] ].
	lineWidth := lineWidth max: width.
	bounds := aPoint extent: lineWidth @ lineHeight.

	self port colorMap: nil.
	portRect := self port clipRect.
	self port
		clipByX1: bounds left + self origin x
		y1: bounds top + self origin y
		x2: bounds right + self origin x
		y2: bounds bottom + self origin y.
	self port combinationRule: Form paint.
	self port fill: bounds fillColor: Color white rule: Form paint.
	font installOn: self port foregroundColor: c backgroundColor: Color white.
	aString lines doWithIndex: [ :line :index |
			font
				displayString: line
				on: self port
				from: 1
				to: line size
				at: bounds topLeft + self origin + (0 @ (index - 1) * textStyle lineGrid)
				kern: 0 ].
	self port clipRect: portRect
]

{ #category : '*Morphic-Base' }
FormCanvas >> roundCornersOf: aMorph in: bounds during: aBlock [
	aMorph wantsRoundedCorners ifFalse:[^aBlock value].
	(self seesNothingOutside: (CornerRounder rectWithinCornersOf: bounds))
		ifTrue: ["Don't bother with corner logic if the region is inside them"
				^ aBlock value].
	CornerRounder roundCornersOf: aMorph on: self in: bounds
		displayBlock: aBlock
		borderWidth: aMorph borderWidthForRounding
		corners: aMorph roundedCorners
]

{ #category : '*Morphic-Base' }
FormCanvas >> roundShadowCornersOf: aMorph in: bounds during: aBlock [
	aMorph wantsRoundedCorners ifFalse:[^aBlock value].
	(self seesNothingOutside: (CornerRounder rectWithinCornersOf: bounds))
		ifTrue: ["Don't bother with corner logic if the region is inside them"
				^ aBlock value].
	CornerRounder roundShadowCornersOf: aMorph on: self in: bounds
		displayBlock: aBlock
		borderWidth: aMorph borderWidthForRounding
		corners: aMorph roundedCorners
]
