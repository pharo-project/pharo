halos and balloon help
hasHalo: aBool
	aBool
		ifTrue:[self setProperty: #hasHalo toValue: true]
		ifFalse:[self removeProperty: #hasHalo]