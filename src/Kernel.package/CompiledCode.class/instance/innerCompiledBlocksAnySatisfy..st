literals
innerCompiledBlocksAnySatisfy: aBlock
	self innerCompiledBlocksDo: [ :cb | (aBlock value: cb) ifTrue: [ ^ true ] ].
	^ false