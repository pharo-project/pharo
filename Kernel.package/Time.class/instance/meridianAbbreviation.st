accessing
meridianAbbreviation

	^ self hour < 12 ifTrue: ['AM'] ifFalse: ['PM']. 