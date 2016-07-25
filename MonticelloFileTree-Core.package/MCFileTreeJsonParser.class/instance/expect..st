private
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^ (self match: aString) ifFalse: [ self error: aString , ' expected' ]