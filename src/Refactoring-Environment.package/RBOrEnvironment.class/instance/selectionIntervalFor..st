accessing
selectionIntervalFor: aString
	^ (environment selectionIntervalFor: aString)
		ifNil: [ orEnvironment selectionIntervalFor: aString ]