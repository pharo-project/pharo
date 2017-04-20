formatting
underlined: aString

	"Force this whole text to be bold."
	aString size = 0 ifTrue: [^self].
	^ aString asText addAttribute: TextEmphasis underlined from: 1 to: aString size.
