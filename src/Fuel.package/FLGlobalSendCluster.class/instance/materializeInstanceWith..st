serialize/materialize
materializeInstanceWith: aDecoder

	| global selector |
	global := (self materializeGlobalAssociationFrom: aDecoder) value.	
	selector := aDecoder nextEncodedString asSymbol.
	^ global perform: selector