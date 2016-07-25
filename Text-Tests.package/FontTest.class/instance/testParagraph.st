testing
testParagraph
	"self debug: #testParagraph"
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
	self assert: p extent = (width @ height)