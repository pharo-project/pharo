as yet unclassified
suite
	^ (Smalltalk hasClassNamed: #MczInstaller)
		ifTrue: [ super suite ]
		ifFalse: [ self classForTestSuite new name: self name asString ]