scanning
readsRef: literalAssociation 
	"Answer whether the receiver loads the argument."
	(self localReadsRef: literalAssociation) ifTrue: [ ^ true ].
	^ self innerCompiledBlocksAnySatisfy: [ :cb | cb readsRef: literalAssociation ]