testing
includesPackageNamed: packageName
	"Verifies if a child package with name = packageName exists"
	
	^self includesElementNamed: packageName in: self packages