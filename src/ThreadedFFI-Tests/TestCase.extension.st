Extension { #name : #TestCase }

{ #category : #'*ThreadedFFI-Tests' }
TestCase >> skipOnAppleARM [

	(self onPharoCITestingEnvironment and: [ 
		 OSPlatform current isMacOS and: [ 
			 Smalltalk vm architectureName = 'arm64' ] ]) ifTrue: [ 
		self skip ]
]
