activation
activate
	self activateHelp
		ifTrue: [  ^ self ].
	
	((self hasOption: 'delete-old') 
		ifTrue: [ self arguments size = 2 ]
		ifFalse: [ self arguments size = 1])
			ifFalse: [ 
				self printHelp.
				^ self exitFailure: 'Missing Arguments' ].
		
	self saveImage.
		