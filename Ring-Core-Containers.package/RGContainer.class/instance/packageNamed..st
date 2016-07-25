lookup by name
packageNamed: packageName
	"Look for a child package named packageName"
	
	^self elementNamed: packageName in: self packages