adding
addNullLineWithIndex: index andRectangle: r

	lines addLast: (
		(
			TextLine 
				start: index 
				stop: index - 1
				internalSpaces: 0 
				paddingWidth: 0
		)
			rectangle: r;
			lineHeight: defaultLineHeight baseline: theTextStyle baseline
	)
