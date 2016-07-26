serialize/materialize
materializeGlobalAssociationFrom: aDecoder

	| globalName |
	globalName := aDecoder nextEncodedString asSymbol.
	^ aDecoder globalEnvironment
		associationAt: globalName
		ifAbsent: [ FLGlobalNotFound signalWithName: globalName ]
