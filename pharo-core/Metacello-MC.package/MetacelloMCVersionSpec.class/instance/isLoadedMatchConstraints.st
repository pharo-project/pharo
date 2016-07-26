testing
isLoadedMatchConstraints
	"all loaded projects and packages match constraints (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedMatchConstraints: [ :ignored | ^ true ].
	^ false