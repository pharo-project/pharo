activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
	
	self selectSourceFiles.
	self installSourceFiles.
	
	self end