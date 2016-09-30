scanning
writesRef: literalAssociation 
	"Answer whether the receiver stores into the argument."
	(self localWritesRef: literalAssociation) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb writesRef: literalAssociation ]