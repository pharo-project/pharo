accessing
mixedMethod

	LocalRecursionStopper during: [ 
		self assert: value equals: 0.
		value := value + 1.
		LocalRecursionStopper during: [ 
			value := value + 1.
		].
		self assert: value equals: 1.
		self mixedMethod.
		self assert: value equals: 1.		
	].
	
	LocalRecursionStopper during: [ 
		self assert: value equals: 1.
		^ 2
	].

	self assert: value equals: 1.
	^ value 