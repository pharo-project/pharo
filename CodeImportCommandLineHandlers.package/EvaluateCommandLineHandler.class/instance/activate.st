activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
		
	self arguments 
		ifEmpty: [ ^ self evaluateStdIn ].
		
	self evaluateArguments.
	
	(self hasOption: 'save')
		ifTrue: [ 
			^ Smalltalk 
				snapshot: true 
				andQuit: (self hasOption: 'no-quit') not ].
	
	(self hasOption: 'no-quit')
		ifFalse: [ self quit ]