compute
currentVersionsToBeSaved
	"self new currentVersionsToBeSaved"
	
	^ self allCurrentVersions reject: [ :each |
		self packagesNotToSavePatternNames
			anySatisfy: [ :p | each matchesRegex: p ] ]