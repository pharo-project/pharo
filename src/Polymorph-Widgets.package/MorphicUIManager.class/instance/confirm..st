ui requests
confirm: aStringOrText
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."
	
	^ self confirm: aStringOrText label: 'Question' translated.