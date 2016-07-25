testing
isLoadedToSpec
	"all loaded projects and packages match specifications (at least one package loaded)"
	
	self isPartiallyCurrent isLoadedToSpec: [ :ignored | ^ true ].
	^ false