initialization
initializeTypeTable

	| newTable |
	newTable := Array new: 256 withAll: #xBinary. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.

	1 to: 255
		do: [:index |
			(Character value: index) isLetter
				ifTrue: [newTable at: index put: #xLetter]].

	newTable at: 30 put: #doIt.
	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #xLetter. "by default, do not accept _ as assignement"
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable := newTable "bon voyage!"
