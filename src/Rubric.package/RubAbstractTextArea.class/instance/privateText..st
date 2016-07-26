accessing text
privateText: stringOrText
	"Accept new text contents."

	| fontNumber |
	stringOrText isText
		ifTrue: [ self paragraph text: stringOrText ]
		ifFalse: [ 
			fontNumber := self textStyle defaultFontIndex.
			self paragraph text: (Text string: stringOrText asString attributes: {(TextFontChange fontNumber: fontNumber)}) ]