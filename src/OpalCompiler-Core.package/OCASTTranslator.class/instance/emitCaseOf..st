inline messages
emitCaseOf: aMessageNode

	self 
		emitCaseOf: aMessageNode 
		otherwiseBlock: [ 
			methodBuilder pushReceiver.
			methodBuilder send: #caseError ]