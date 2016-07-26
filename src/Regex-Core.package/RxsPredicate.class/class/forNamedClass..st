instance creation
forNamedClass: aString

	^self new perform:
		(NamedClassSelectors
			at: aString
			ifAbsent: [RxParser signalSyntaxException: 'bad character class name'])