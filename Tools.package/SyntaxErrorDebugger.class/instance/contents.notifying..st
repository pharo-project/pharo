other
contents: aString notifying: aController
	"Compile the code in aString and proceed. Do not notify anybody of errors, because nobody would have been notified of errors if this syntax error had not arisen"

	aController hasUnacceptedEdits: false.
	
	doitFlag
		ifTrue: [ self class compiler evaluate: aString ]
		ifFalse: [ self resume: aString ].
	self closeWindow