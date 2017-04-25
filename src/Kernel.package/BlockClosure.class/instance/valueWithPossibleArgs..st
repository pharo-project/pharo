evaluating
valueWithPossibleArgs: anArray 
	"Execute the receiver with the correct number of arguments taken from the argument."
	"([:x | x + 1] valueWithPossibleArgs: #( 13 12 15))
	>>> 14
	"
	"([:x :y | x + y] valueWithPossibleArgs: #( 13 12 15)) 
	>>> 25
	"
	"([:x :y :z | x + y + z] valueWithPossibleArgs: #( 13 12 15)) 
	>>> 40
	"
	"([:x :y :z | x + y + z] valueWithPossibleArgs: #( 13 12 15)) 
	>>> 40
	"
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