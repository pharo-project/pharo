classification
classify: aMethod
	(self classifyAccessor: aMethod)
		ifTrue: [ ^ aMethod category ].
	(self classifyInSuperclassProtocol: aMethod)
		ifTrue: [ ^ aMethod category ].
		
	(self classifyByKnownPrefix: aMethod)
		ifTrue: [ ^ aMethod category ].
	(self classifyByOtherImplementors: aMethod)
		ifTrue: [ ^ aMethod category ].
	(self classifyByKnownPragma: aMethod)
		ifTrue: [ ^ aMethod category ].