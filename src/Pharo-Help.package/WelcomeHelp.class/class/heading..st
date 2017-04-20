formatting
heading: aString

	| font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize: 20).
	color := Color fromHexString: '3196D3'.

	aString size = 0 ifTrue: [^self].
	^ (aString asText, String cr, String cr)
		addAttribute: (TextFontReference toFont: font) from: 1 to: aString size;
		addAttribute: TextEmphasis bold from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		yourself
