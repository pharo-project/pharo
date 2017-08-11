activation
activate
	| aClass aMessage arguments messageArguments |
	self activateHelp
		ifTrue: [ ^ self ].
	arguments := self commandLine arguments reject: [ :argument | argument beginsWith: '-' ].
	(arguments size <= 2)
		ifFalse: [ ^ self exitFailure: 'Not enough arguments' ].
		
	aClass := self class environment globals at: arguments first asSymbol ifAbsent: [ 
		  ^ self exitFailure: 'Unknown class' ].
	aMessage := arguments second.
	messageArguments := arguments allButFirst: 2.
	
	aClass perform: aMessage withArguments: messageArguments.
	
	(self hasOption: 'save')
		ifTrue: [ Smalltalk snapshot: true andQuit: false ].
	self exitSuccess