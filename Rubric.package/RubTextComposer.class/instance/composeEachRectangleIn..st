private composition
composeEachRectangleIn: rectangles

	| myLine lastChar |

	1 to: rectangles size do: [:i | 
		currCharIndex <= theText size ifFalse: [ ^ false ].
		myLine := scanner 
			composeFrom: currCharIndex 
			inRectangle: (rectangles at: i)				
			firstLine: isFirstLine 
			leftSide: i=1 
			rightSide: i=rectangles size.
		myLine actualWidth: scanner rightX - theContainer left.
		
		lines addLast: myLine.
		actualHeight := actualHeight max: myLine lineHeight.  "includes font changes"
		currCharIndex := myLine last + 1.
		
		"This happens if the text is just one line long"
		(myLine last = 0) ifTrue: [ ^ false ].
		
		lastChar := theText at: myLine last.
		(CharacterSet crlf includes: lastChar) ifTrue: [^#cr].
	].
	^false