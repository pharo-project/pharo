reflectivity
emitMetaLinkAfter: aNode
	aNode hasMetalinkAfter ifFalse: [ ^self ].
	aNode allAfterAreWeak ifTrue: [ ^self emitMetaLinkAfterNoEnsure: aNode ].
	
	methodBuilder blockReturnTop.
	methodBuilder jumpAheadTarget: #block.		
	valueTranslator visitNode: aNode afterHooks.
	methodBuilder send: #ensure:.