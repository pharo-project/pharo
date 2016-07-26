activation
activate
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	self getProject