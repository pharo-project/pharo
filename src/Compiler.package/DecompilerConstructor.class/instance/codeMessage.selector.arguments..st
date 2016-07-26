constructor
codeMessage: receiver selector: selector arguments: arguments
	| symbol node |
	symbol := selector key.
	(node := BraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	(node := self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	^ MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence