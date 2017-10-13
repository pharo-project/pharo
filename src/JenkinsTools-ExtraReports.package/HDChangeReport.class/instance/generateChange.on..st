generating
generateChange: aPatch on: aStream
	aPatch isAddition
		ifTrue: [ ^ self generateChangeAddition: aPatch on: aStream ].
	aPatch isModification
		ifTrue: [ ^ self generateChangeModification: aPatch on: aStream ].
	aPatch isRemoval
		ifTrue: [ ^ self generateChangeRemoval: aPatch on: aStream ]