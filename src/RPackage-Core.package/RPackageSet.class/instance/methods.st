accessing
methods
	^methods ifNil: [ methods := (self collectFromAllPackages: #methods) collect: #asRingDefinition ]