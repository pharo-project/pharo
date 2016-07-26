waiting-specific
waitWhile: aBlock for: aSymbolOrNil
	"Confitional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific 
	event and not the default event."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: nil