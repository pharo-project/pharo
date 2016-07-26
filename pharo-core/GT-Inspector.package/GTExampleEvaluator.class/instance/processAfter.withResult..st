private
processAfter: anExample withResult: anObject
	anExample after ifNil: [ ^ self ].
	anExample after method
		valueWithReceiver: anExample after provider 
		possibleArguments: { anObject }