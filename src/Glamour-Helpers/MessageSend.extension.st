Extension { #name : #MessageSend }

{ #category : #'*glamour-helpers' }
MessageSend >> glamourValueWithArgs: anArray [ 

	
	self selector isUnary ifTrue: [^self value]. 
	self selector numArgs = anArray size ifTrue: [^self valueWithArguments: anArray]. 
	self selector numArgs > anArray size 
		ifTrue: 
			[^self 
				valueWithArguments: anArray , (Array new: self selector numArgs - anArray size)]. 
	^self 
		valueWithArguments: 
			(anArray 
				copyFrom: 1 
				to: self selector numArgs)
]
