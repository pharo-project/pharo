accessing
configuration
	configuration ifNil: [self loadConfiguration].
	"browser modifies configuration, but the reader might get cached"
	^configuration copy