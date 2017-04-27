formatting
bold: aString
	"Return Text object with bold emphasis."
	aString size = 0 ifTrue: [^self].
	^ aString asText addAttribute: TextEmphasis bold from: 1 to: aString size.
