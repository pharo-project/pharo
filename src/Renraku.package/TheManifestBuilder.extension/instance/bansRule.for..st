*renraku
bansRule: aRule for: anEntity
	| name number |
	  name := aRule class name.
	number := aRule class identifierMinorVersionNumber.
	
	^ self
		containsFalsePositive: anEntity onRule: name version: number