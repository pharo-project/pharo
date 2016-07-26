subclass responsibilities
systemConverterClass

	Smalltalk os isWin32
		ifTrue: [^CP1253TextConverter ].
	^ ISO88597TextConverter.
