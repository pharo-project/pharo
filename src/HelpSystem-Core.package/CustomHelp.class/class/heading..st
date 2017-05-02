formatting
heading: aString
	"Return Text object with heading formating attributes."

	| font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize: 20).
	color := Color fromHexString: '3196D3'.

	aString ifEmpty: [ self errorEmptyString ].
	^ (aString asText, String cr, String cr)
		addAttribute: (TextFontReference toFont: font) from: 1 to: aString size;
		addAttribute: TextEmphasis bold from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		yourself
