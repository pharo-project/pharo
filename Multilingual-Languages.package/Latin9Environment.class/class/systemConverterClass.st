subclass responsibilities
systemConverterClass

	Smalltalk os isWin32 
		ifTrue: [^CP1252TextConverter ].

	^ ISO885915TextConverter.
