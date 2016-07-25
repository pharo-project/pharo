accessing
resources
	^preventAvailability isNil
		ifTrue: [super resources , (Array with: SimpleTestResourceA1)]
		ifFalse:
			[super resources
				, (Array with: SimpleTestResourceA1 with: SimpleTestResourceCircular1)]