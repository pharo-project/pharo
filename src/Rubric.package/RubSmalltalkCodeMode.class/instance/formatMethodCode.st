initialize-release
formatMethodCode
	| source tree formatted |
	source := self textArea text asString.
	tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
	formatted := tree formattedCodeWithMaxLineLength: self maxFormatterLineLength.
	formatted = source
		ifTrue: [ ^ self ].
	self textArea updateTextWith: formatted