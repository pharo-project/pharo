Extension { #name : #BlockClosure }

{ #category : #'*Glamour-Helpers' }
BlockClosure >> glamourValueWithArgs: anArray [ 
	"This is the same as valueWithPossibleArgs: from squeak. 
	Unfortunately, VW's cullValue: is incompatible." 
	 
	self numArgs = 0 ifTrue: [^self value]. 
	self numArgs = anArray size ifTrue: [^self valueWithArguments: anArray]. 
	self numArgs > anArray size 
		ifTrue: 
			[^self 
				valueWithArguments: anArray , (Array new: self numArgs - anArray size)]. 
	^self 
		valueWithArguments: 
			(anArray 
				copyFrom: 1 
				to: self numArgs)
]
