serialize/materialize
materializeGlobalClassFrom: aDecoder

	| className |
	className := aDecoder nextEncodedString asSymbol.
	^ aDecoder classNamed: className