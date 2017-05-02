formatting
bold: aString
	"Return Text object with bold emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	^ aString asText addAttribute: TextEmphasis bold from: 1 to: aString size.
