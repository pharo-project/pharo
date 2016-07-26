accessing
headlessManager
	self class == CommandLineUIManager 
		ifFalse: [ ^ self ].
		
	^ CommandLineUIManager replacing: uiManager