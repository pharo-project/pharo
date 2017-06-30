tests
testClassNotFound
	
	| correctErrorRaised |
	
	correctErrorRaised := false.
	
	[ Smalltalk globals classNamed: #notExistedClass ]
		on: ClassNotFound do: [
			correctErrorRaised := true. ].
		
    self assert: correctErrorRaised.