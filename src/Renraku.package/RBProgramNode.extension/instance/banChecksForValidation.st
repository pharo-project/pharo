*renraku
banChecksForValidation
"returns ban levels that have to be chacked before validation"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banChecksForValidation ] ].
	
	^ #()