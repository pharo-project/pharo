reflectivity
emitMetaLinkBefore: aNode
	aNode hasMetalinkBefore ifFalse: [ ^self ].
	self emitPreamble: aNode.
	aNode beforeHooks do: [ :hook | effectTranslator visitNode: hook ].