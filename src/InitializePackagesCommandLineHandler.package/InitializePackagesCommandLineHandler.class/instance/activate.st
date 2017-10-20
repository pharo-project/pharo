activation
activate

	self activateHelp
		ifTrue: [ ^ self ].
		
	(self hasOption: 'protocols')
			ifTrue: [ self initializeProtocolsFrom: (self optionAt: 'protocols')].
				
	(self hasOption: 'packages')
			ifTrue: [ self initializePackagesFrom: (self optionAt: 'packages')].
				
	(self hasOption: 'save')
		ifTrue: [ Smalltalk snapshot: true andQuit: false ].
	self exitSuccess