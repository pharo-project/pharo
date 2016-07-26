testing
isFromSharedPool
	| sharedPools |
	sharedPools := scope getClass baseClass sharedPools.
	^ sharedPools 
		detect: [ :pool | pool classVarNames includes: assoc key ]
		ifFound: [ true ]
		ifNone: [ false ]