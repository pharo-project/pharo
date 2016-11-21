instance creation
methodFor: dataAndAnswers
	"Return an expression that computes these answers."

	| resultOC resultString |
	resultOC := self new load: dataAndAnswers; findMessage.
	resultString := String streamContents: [:strm |
		resultOC sorted do: [:exp | strm nextPut: $(; nextPutAll: exp; nextPut: $); space]].
	^ resultString