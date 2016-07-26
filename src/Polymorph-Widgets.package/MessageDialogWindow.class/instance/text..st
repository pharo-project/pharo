accessing
text: aStringOrText
	"Set the text."
	
	|t|
	t := aStringOrText isString
		ifTrue: [aStringOrText asText addAttribute: (TextFontReference toFont: self textFont); yourself]
		ifFalse: [aStringOrText].
	t
		addAttribute: TextAlignment centered;
		addAttribute: (TextColor color: self textMorph textColor).
	self textMorph newContents: t
