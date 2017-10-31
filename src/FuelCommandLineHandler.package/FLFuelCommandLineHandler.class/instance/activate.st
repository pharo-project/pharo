activation
activate
	self manageFiles.
	(self commandLine hasOption: 'save')
		ifTrue: [
			Smalltalk addDeferredStartupAction: [
				Smalltalk snapshot: true andQuit: true.
			]
		].
	(self commandLine hasOption: 'quit')
		ifTrue: [ self exitSuccess ].