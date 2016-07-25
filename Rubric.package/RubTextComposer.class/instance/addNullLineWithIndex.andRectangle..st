adding
addNullLineWithIndex: index andRectangle: aRectangle
	| nullString nullLine |
	nullString := ' ' asText.
	self emphasisHere do: [ :attr | nullString addAttribute: attr ].
	scanner text: nullString textStyle: theTextStyle.
	nullLine := scanner
		composeFrom: 1
		inRectangle: aRectangle
		firstLine: true
		leftSide: true
		rightSide: false.
	nullLine firstIndex: index lastIndex: index - 1.
	nullLine internalSpaces: 0 paddingWidth: 0.
	nullLine rectangle: ( aRectangle withBottom: nullLine bottom).
	lines addLast: nullLine 
