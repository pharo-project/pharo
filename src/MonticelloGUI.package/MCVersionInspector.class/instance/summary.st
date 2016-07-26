accessing
summary
	^self hasVersion
		ifTrue: [ self versionSummary ]
		ifFalse: [ String new ]