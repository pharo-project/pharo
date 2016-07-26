testing
isSomethingLoaded
	"at least one package loaded"
	
	self isPartiallyCurrent isSomethingLoaded: [ :ignored | ^ true ].
	^ false