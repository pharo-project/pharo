api
formatSourceCode
	| source tree formatted |
	source := self compiledMethodFromSearchFields sourceCode.
	tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
	formatted := tree
		formattedCodeWithMaxLineLength: (self window value window value width / 9) integerPart asInteger.	"It is a bit tricky but I get the width of the sourceCodePane like this..."
	sourceCodePane text: formatted