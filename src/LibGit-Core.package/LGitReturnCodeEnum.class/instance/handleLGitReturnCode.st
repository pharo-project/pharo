handling
handleLGitReturnCode
	| handler |
	self isOk
		ifTrue: [ ^ self ].
	handler := self handlers
		at: self symbol
		ifAbsent: [ LGitNoReturnCodeHandlerDefined signalWith: self ].
	^ handler isError
		ifTrue: [ handler signalWith: LGitError last ]
		ifFalse: [ self ]