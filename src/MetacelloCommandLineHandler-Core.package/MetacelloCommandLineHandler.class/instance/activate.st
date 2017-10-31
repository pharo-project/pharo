activation
activate
	self activateHelpWithoutArguments
		ifTrue: [ ^ self ].
	self subCommand = 'install'
		ifTrue: [ 
			self
				checkInputForInstallSubcommand;
				install;
				postInstall ]