helper
handleFailure: anError of: aTestCase 
	(anError isNil or: [aTestCase isExpectedFailure])  ifTrue: [ ^ self ].

	(anError isKindOf: TestFailure)
		ifTrue: [ 
			suiteFailures := suiteFailures + 1.
			self printFailure: anError of: aTestCase ]
		ifFalse: [ 
			suiteErrors := suiteErrors + 1.
			self printError: anError of: aTestCase ].
	
	self shouldSerializeError 
		ifTrue: [ self serializeError: anError of: aTestCase ]