activation
activate
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	
	self subCommand = 'install'
		ifTrue: [ 
			(self isBaselineName or: [ self isConfigurationName ])
				ifFalse: [ Exit signalFailure: 'Not a Baseline nor a Configuration.' ].
			self
				install;
				postConfiguration ]