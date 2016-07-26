examples
exampleTextMultiSizeFont

	| text |
	text := TxModel new.
	100 timesRepeat: [ 	
		1 to: 10 do: [ :i | | font lastLineSelection | 
			text endPosition insert: 'The quick brown fox jumps over the lazy dog 1234567890'.
			font := LogicalFont familyName: self fontFamilyForExamples pointSize: 9 + i.
			lastLineSelection := text endPosition moveToLineStart selectTo: text endPosition.
			lastLineSelection applyAttribute: (TxFontAttribute with: font).
			text endPosition insert: String cr.
		].
	].
	^ self openInWindowWithText: text