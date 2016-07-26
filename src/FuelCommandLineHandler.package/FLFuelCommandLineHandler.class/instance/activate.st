activation
activate
	self manageFiles.
	(self commandLine hasOption: 'save')
		ifTrue: [
			ThreadSafeTranscript install.
			Smalltalk addDeferredStartupAction: [
				Smalltalk snapshot: true andQuit: true.
				NonInteractiveTranscript new install.
			]
		].
	(self commandLine hasOption: 'quit')
		ifTrue: [ self exitSuccess ].