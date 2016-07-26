*glamour-helpers
glamourValueWithArgs: anArray 
	"This is the same as valueWithPossibleArgs: from squeak. 
	Unfortunately, VW's cullValue: is incompatible." 
	
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