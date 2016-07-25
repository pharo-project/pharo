activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
	self runPackages.