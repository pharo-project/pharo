classification
classify: aMethod

	(self classifyInSuperclassProtocol: aMethod)
		ifTrue: [ ^ aMethod protocol ].
	(self classifyAccessor: aMethod)
		ifTrue: [ ^ aMethod protocol ].
	(self classifyByKnownPrefix: aMethod)
		ifTrue: [ ^ aMethod protocol ].
	(self classifyByKnownKeywordSuffix: aMethod)
		ifTrue: [ ^ aMethod protocol ].
	(self classifyByKnownPragma: aMethod)
		ifTrue: [ ^ aMethod protocol ].		
	(self classifyByOtherImplementors: aMethod)
		ifTrue: [ ^ aMethod protocol ].


