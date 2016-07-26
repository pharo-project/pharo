subclass responsibilities
systemConverterClass

	Smalltalk os isWin32 
		ifTrue: [^CP1250TextConverter ].

	^ ISO88592TextConverter.
