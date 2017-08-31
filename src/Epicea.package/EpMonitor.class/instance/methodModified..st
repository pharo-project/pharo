announcement handling
methodModified: aMethodModified

	self handleAnyErrorDuring: [
		(aMethodModified oldMethod sourceCode = aMethodModified newMethod sourceCode 
			and: [ aMethodModified oldProtocol = aMethodModified newProtocol ])
			ifTrue: [ ^self ].
			
		self addEvent: 
			(EpMethodModification
				oldMethod: aMethodModified oldMethod
				newMethod: aMethodModified newMethod) ]