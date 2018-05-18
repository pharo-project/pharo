"
I am mainly a test for fallback font.
FontTest buildSuite run
"
Class {
	#name : #FontTest,
	#superclass : #TestCase,
	#category : #'Text-Tests-Fonts'
}

{ #category : #testing }
FontTest >> testDisplay [
	
	| text font bb destPoint width |
	text := 'test' asText.
	font := TextStyle default fontOfSize: 21.
	text addAttribute: (TextFontReference toFont: font).
	bb := (Form extent: 100 @ 30) getCanvas privatePort.
	bb combinationRule: Form paint.

	font installOn: bb foregroundColor: Color black backgroundColor: Color white.
	destPoint := font displayString: text on: bb from: 1 to: 4 at: 0@0 kern: 1.

	width := text inject: 0 into: [:max :char | max + (font widthOf: char)].
	self assert: destPoint x equals: (width + 4)
	"bb destForm asMorph openInHand."

]

{ #category : #testing }
FontTest >> testParagraph [
	
	| text p style height width |
	text := 'test' asText.
	p := Paragraph new.
	style := TextStyle default.
	p
		compose: text
		style: style
		from: 1
		in: (0 @ 0 corner: 100 @ 100).
	"See CompositionScanner>>setActualFont: &  
	CompositionScanner>>composeFrom:inRectangle:firstLine:leftSide:rightSide:"
	height := style defaultFont height + style leading.
	width := text
				inject: 0
				into: [:tally :next | tally
						+ (style defaultFont widthOf: next)].
	p adjustRightX.
	self assert: p extent equals: (width @ height)
]
