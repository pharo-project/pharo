activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
	self cleanUpImage.
	Smalltalk snapshot: true andQuit: true.