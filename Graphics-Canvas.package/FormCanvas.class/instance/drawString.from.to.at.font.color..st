drawing-text
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: c
	| font |
	port colorMap: nil.
	font := fontOrNil ifNil: [TextStyle defaultFont].
	port combinationRule: Form paint.
	font installOn: port
		foregroundColor: c
		backgroundColor: Color transparent.
	font displayString: aString on: port 
		from: firstIndex to: lastIndex at: (origin + aPoint) kern: 0.