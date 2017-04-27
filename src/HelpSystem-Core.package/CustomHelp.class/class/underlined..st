formatting
underlined: aString
	"Return Text object with underlined emphasis."
	aString size = 0 ifTrue: [^self].
	^ aString asText addAttribute: TextEmphasis underlined from: 1 to: aString size.
