package filter
useLastPackagePatternStringForClass: aClass
	| patternString |
	patternString := SharedPackageFilterPatternsHolder instance lastValue.
	patternString ifNil: [ ^ self ].
	(self class: aClass matchPackagePatternString: patternString)
		ifFalse: [ patternString := patternString, '|', aClass package name ].
	self packagePatternString: patternString.