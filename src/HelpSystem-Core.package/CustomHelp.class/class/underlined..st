formatting
underlined: aString
	"Return Text object with underlined emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	
	^ aString asText addAttribute: TextEmphasis underlined from: 1 to: aString size.
