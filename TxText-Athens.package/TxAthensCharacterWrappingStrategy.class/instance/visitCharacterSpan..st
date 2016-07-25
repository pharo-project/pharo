visitor
visitCharacterSpan: aSpan
	
	| chars pos startPos font w fontMetricsProvider viewWidth  glyphWidth |
	
	chars := aSpan characters.
	
	startPos := pos := currentPosition spanPosition.

	font := aSpan getAttribute: TxFontAttribute.  
	
	fontMetricsProvider := self fontMetricsProviderFor: font.
	currentLine accumulateHeight: fontMetricsProvider fontHeight.
	currentLine accumulateAscent: fontMetricsProvider fontAscent.
	
	viewWidth := self view extent x.
	glyphWidth := w := 0.
	startPos := pos+1.
	
	[ pos < chars size ] whileTrue: [ | char |
	
		char := chars at: pos + 1.
		char == Character tab ifTrue: [ | ta | 
			ta := self alignTab: font.
			((currentLine width + ta x > (viewWidth - rightMargin)) and: [ currentLine width >0 ]) ifTrue: [ 
				"Need to wrap"
				
				currentLine addCommand: (
					fontMetricsProvider 
						newCharacterSpanDisplayCommand: aSpan 
						start: startPos 
						stop: pos 
						advance: w@0).

				currentPosition := aSpan positionAt: pos.
				self completeLine.
				^ currentPosition
				].
				
			currentLine addCommand: (
				fontMetricsProvider 
					newCharacterSpanDisplayCommand: aSpan 
					start: startPos 
					stop: pos
					advance: w@0).

			
			currentLine addWidth: ta x.
			currentLine addCommand: (fontMetricsProvider 
						newTabDisplayCommand: aSpan
						start: pos-1
						stop: pos
						advance: ta).
				currentPosition := aSpan positionAt: pos+1.
				^ currentPosition
			].
		
		
		glyphWidth := (fontMetricsProvider getGlyphWidth: char).
		"Do not wrap unless we put at least one character into current line"
		((currentLine width + glyphWidth > (viewWidth - rightMargin)) and: [ currentLine width >0 ]) ifTrue: [ 
				"wrap to new line"
				currentLine addCommand: (
					fontMetricsProvider 
						newCharacterSpanDisplayCommand: aSpan 
						start: startPos 
						stop: pos 
						advance: w@0).

				currentPosition := aSpan positionAt: pos.
				self completeLine.
				^ currentPosition
			] 
		ifFalse: [ 
			currentLine addWidth: glyphWidth.
			w := w + glyphWidth.
			pos := pos +1
		]
	].

	currentLine addCommand: (
		fontMetricsProvider 
			newCharacterSpanDisplayCommand: aSpan 
			start: startPos 
			stop: chars size
			advance: w@0).
			
	^ aSpan next startPosition
