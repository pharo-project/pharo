exceptiondescription
messageText
	"Return an exception's message text."

	^ messageText isNil
		ifTrue: [ fileName printString ]
		ifFalse: [ messageText ]