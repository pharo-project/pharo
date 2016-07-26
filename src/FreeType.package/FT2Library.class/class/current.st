instance creation
current
	^[ (self basicNew)
		current ] on: FT2Error do: [ :ex | ex return: nil ].