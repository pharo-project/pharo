Extension { #name : #FormCanvas }

{ #category : #'*Morphic-Base' }
FormCanvas >> asBalloonCanvas [
	^(BalloonCanvas on: form) setOrigin: origin clipRect: clipRect
]

{ #category : #'*Morphic-Base' }
FormCanvas >> drawString: aString from: firstIndex to: lastIndex autoBoundAt: aPoint font: fontOrNil color: c [

	| font textStyle portRect bounds character width carriageReturn lineWidth lineHeight |

	carriageReturn := Character cr.
	width := lineWidth := 0.
	font := StandardFonts codeFont.
	textStyle := font textStyle.
	lineHeight := textStyle lineGrid.
	1 to: aString size do: 
		[:i | 
		character := aString at: i.
		character = carriageReturn
		  ifTrue: 
			[lineWidth := lineWidth max: width.
			lineHeight := lineHeight + textStyle lineGrid.
			width := 0]
		  ifFalse: [width := width + (font widthOf: character)]].
	lineWidth := lineWidth max: width.
	bounds := aPoint extent: (lineWidth  @ lineHeight).
	
	port colorMap: nil.
	portRect := port clipRect.
	port clipByX1: bounds left + origin x 
		y1: bounds top + origin y 
		x2: bounds right + origin x 
		y2: bounds bottom + origin y.
	port combinationRule: Form paint.
	port fill: bounds fillColor: Color white rule: Form paint.
	font installOn: port
		foregroundColor: c
		backgroundColor: Color white.
	aString lines doWithIndex: [:line :index |
		font displayString: line on: port 
			from: 1 to: line size at: (bounds topLeft + origin + (0@(index-1)*textStyle lineGrid)) kern: 0.
	].
	port clipRect: portRect.
]

{ #category : #'*Morphic-Base' }
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

{ #category : #'*Morphic-Base' }
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
