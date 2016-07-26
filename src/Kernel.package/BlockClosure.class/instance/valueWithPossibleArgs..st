evaluating
valueWithPossibleArgs: anArray 

	^numArgs = 0
		ifTrue: [self value]
		ifFalse:
			[self valueWithArguments:
				(numArgs = anArray size
					ifTrue: [anArray]
					ifFalse:
						[numArgs > anArray size
							ifTrue: [anArray, (Array new: numArgs - anArray size)]
							ifFalse: [anArray copyFrom: 1 to: numArgs]])]