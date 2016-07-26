package filter
packageSearchUpdate: aPatternString
	packagePatternString = aPatternString ifTrue: [ ^ self ].
	packagePatternString := aPatternString.
	packagePattern := self createPackagePattern: aPatternString.
	SharedPackageFilterPatternsHolder instance lastValue: aPatternString.
	self updatePackages