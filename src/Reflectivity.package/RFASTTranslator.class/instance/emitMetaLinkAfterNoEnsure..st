reflectivity
emitMetaLinkAfterNoEnsure: aNode

	aNode hasMetalinkAfter ifFalse: [ ^self ].
	self emitPreamble: aNode.
	aNode afterHooks do: [ :each | effectTranslator visitNode: each body ].