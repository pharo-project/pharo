correcting
declareClassVar

	self methodClass theNonMetaClass
		addClassVarNamed: node name asSymbol.
	^ self lookUpVariable