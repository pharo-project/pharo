announcement handling
methodModified: aMethodModified

	(aMethodModified oldMethod sourceCode = aMethodModified newMethod sourceCode 
		and: [ aMethodModified oldProtocol = aMethodModified newProtocol ])
		ifTrue: [ ^self ].
		
	self addEvent: 
		(EpMethodModification
			oldMethod: aMethodModified oldMethod
			newMethod: aMethodModified newMethod)