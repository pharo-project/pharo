accessing
classNamed: aName
	^ environment at: aName asSymbol ifAbsent:[nil].