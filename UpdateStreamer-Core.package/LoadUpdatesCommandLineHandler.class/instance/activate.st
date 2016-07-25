activation
activate
	self activateHelp
		ifTrue: [  ^ self ].

	self loadUpdates.

	Smalltalk snapshot: true andQuit: true.