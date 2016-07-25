drawing
drawOn: aCanvas offset: origin

	| box dayString textColor textTopLeft textWidth today |
	dayString := date dayOfMonth printString.
	textWidth := owner weekdayFont widthOfString: dayString.
	textTopLeft := bounds topCenter translateBy: (textWidth // -2) @ 3.
	box := ((textTopLeft extent: textWidth @ owner weekdayFont height) insetBy: -8 @ -1) translateBy: origin.
	today := date = Date today.
	textColor := date month = owner date month
		ifTrue: [Color black]
		ifFalse: [Color veryLightGray].
	(date = owner date or: [self highlighted])
		ifTrue: [
			| lineColor fillColor |
			lineColor := today
				ifTrue: [Color red]
				ifFalse: [Color veryLightGray].
			fillColor := self highlighted
				ifTrue: [Color veryVeryLightGray]
				ifFalse: [Color veryLightGray].
			aCanvas fillOval: box color: fillColor borderWidth: 1 borderColor: lineColor].
	today & (date ~= owner date) & self highlighted not
		ifTrue: [aCanvas fillOval: box color: Color white borderWidth: 1 borderColor: Color red].
	aCanvas
		drawString: dayString
		at: textTopLeft + origin
		font: owner weekdayFont
		color: textColor.