examples
openInWindowWithString: aString

	| text font |
	
	text := TxModel new.
	font := LogicalFont familyName: self fontFamilyForExamples pointSize: 16.
	text defaultStyle
		add: TxFontAttribute with: font;
		add: TxForeColorAttribute with: Color blue.
	
	text endPosition insert: aString.
	
	^ self openInWindowWithText: text