package filter
createPackagePattern: aPatternString
	^ (aPatternString isNil or: [ aPatternString trimBoth isEmpty ])
		ifTrue: [ nil ]
		ifFalse: [ self ensureSafePackagePattern: aPatternString ]