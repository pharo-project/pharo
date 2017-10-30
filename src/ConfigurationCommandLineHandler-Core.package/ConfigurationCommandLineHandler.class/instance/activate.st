activation
activate
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	
	(self hasOption: 'install')
		ifFalse: [ ^ self list ].
		
	self installConfiguration.
	
	self postConfiguration.
		