accessing
parseStream: aStream
	"Parse an input from a character stream <aStream>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	| tree |
	input := aStream.
	lookahead := nil.
	self match: nil.
	tree := self regex.
	self match: #epsilon.
	^tree