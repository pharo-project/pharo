waiting-specific
waitUntil: aBlock for: aSymbolOrNil
	"Confitional waiting for the non-default event represented by the argument symbol.
	See Monitor>>waitWhile:for: aBlock."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: nil