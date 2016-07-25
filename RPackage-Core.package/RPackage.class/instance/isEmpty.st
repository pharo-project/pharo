testing
isEmpty
	self name = self class defaultPackageName ifTrue: [ ^false ].
	^self classes isEmpty and: [ self extensionSelectors isEmpty]
		