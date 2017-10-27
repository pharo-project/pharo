activation
checkInputForInstallSubcommand
	(self isBaselineName and: [ self hasVersion ])
		ifTrue: [ Exit signalFailure: 'A baseline can not be used with --version option.' ].
	
	(self isBaselineName or: [ self isConfigurationName ])
		ifFalse: [ Exit signalFailure: 'Not a Baseline nor a Configuration.' ]