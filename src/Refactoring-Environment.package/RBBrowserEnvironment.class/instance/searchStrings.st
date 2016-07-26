accessing
searchStrings
	^searchStrings isNil
		ifTrue: [#()]
		ifFalse: [searchStrings]