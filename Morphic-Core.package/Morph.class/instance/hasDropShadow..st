drop shadows
hasDropShadow: aBool
	aBool
		ifTrue:[self setProperty: #hasDropShadow toValue: true]
		ifFalse:[self removeProperty: #hasDropShadow]