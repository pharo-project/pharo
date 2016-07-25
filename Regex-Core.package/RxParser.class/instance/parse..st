accessing
parse: aString
	"Parse input from a string <aString>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	^self parseStream: (ReadStream on: aString)