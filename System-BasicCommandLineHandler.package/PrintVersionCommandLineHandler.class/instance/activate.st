activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
		
	(self hasOption: 'numeric')
		ifTrue: [ 
			self printNumericVersion.
			^ self exitSuccess].
	(self hasOption: 'release')
		ifTrue: [ 
			self printRelease.
			^ self exitSuccess ].
	self hasArguments
		ifFalse: [ 
			self printVersion.
			^ self exitSuccess ].
	
	self printHelp.
	^ self exitFailure