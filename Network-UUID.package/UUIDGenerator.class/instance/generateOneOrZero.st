generator
generateOneOrZero
	^self semaphoreForGenerator
		critical: [
			| value | 
			value := self randomGenerator next.
			self randomCounter: self randomCounter + 1.
			self randomCounter > 100000
				ifTrue: [self setupRandom].
			value < 0.5
					ifTrue: [0]
					ifFalse: [1]]