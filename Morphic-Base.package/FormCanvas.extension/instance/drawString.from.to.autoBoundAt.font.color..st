*Morphic-Base
drawString: aString from: firstIndex to: lastIndex autoBoundAt: aPoint font: fontOrNil color: c

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