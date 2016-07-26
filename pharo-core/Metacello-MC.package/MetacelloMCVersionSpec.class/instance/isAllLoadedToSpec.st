testing
isAllLoadedToSpec
	"all projects and packages are loaded and match specification"

	self isPartiallyCurrent isAllLoadedToSpec: [ :ignored | ^ true ].
	^ false