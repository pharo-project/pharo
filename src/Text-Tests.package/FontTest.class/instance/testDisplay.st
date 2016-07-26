testing
testDisplay
	"self debug: #testDisplay"
	| text font bb destPoint width |
	text := 'test' asText.
	font := TextStyle default fontOfSize: 21.
	text addAttribute: (TextFontReference toFont: font).
	bb := (Form extent: 100 @ 30) getCanvas privatePort.
	bb combinationRule: Form paint.

	font installOn: bb foregroundColor: Color black backgroundColor: Color white.
	destPoint := font displayString: text on: bb from: 1 to: 4 at: 0@0 kern: 1.

	width := text inject: 0 into: [:max :char | max + (font widthOf: char)].
	self assert: destPoint x = (width + 4).
	"bb destForm asMorph openInHand."
