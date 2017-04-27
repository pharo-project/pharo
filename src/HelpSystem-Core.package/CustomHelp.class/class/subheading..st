formatting
subheading: aString
	"Return Text object with subheading formating attributes."

	| text font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize:16).
	color := Color fromHexString: '3196D3'.
	text := (String cr asText, String cr, aString, String cr, String cr).
	
	^ text
		addAttribute: (TextFontReference toFont: font) from: 3 to: text size - 1;
		addAttribute: TextEmphasis bold from: 1 to: text size - 1;
		addAttribute: (TextColor new color: color) from: 1 to: text size - 1;
		yourself
