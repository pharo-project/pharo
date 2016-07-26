accessing
allRules
	^ self rules: (Array with: self lintChecks with: self transformations) name: 'All checks'