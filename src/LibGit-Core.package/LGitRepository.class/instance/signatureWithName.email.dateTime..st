operations
signatureWithName: name email: email dateTime: dateTime
	| signaturePointer time |
	signaturePointer := ExternalAddress new.
	time := LGitTime fromDateAndTime: dateTime.
	self
		withReturnHandlerDo: [ self
				new_signature: signaturePointer
				name: name
				email: email
				time: time ].
	^ LGitSignature fromHandle: signaturePointer