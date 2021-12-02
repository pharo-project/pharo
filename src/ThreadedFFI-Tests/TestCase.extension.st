Extension { #name : #TestCase }

{ #category : #'*ThreadedFFI-Tests' }
TestCase >> skipOnAppleARM [

	(self onPharoCITestingEnvironment and: [ 
		 OSPlatform current isMacOS and: [ 
			 Smalltalk vm architectureName = 'aarch64' ] ]) ifTrue: [ 
		self skip ]
]
