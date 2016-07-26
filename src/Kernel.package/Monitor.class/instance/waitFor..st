waiting-specific
waitFor: aSymbolOrNil
	"Unconditional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>wait, but the process gets only reactivated by the specific event and 
	not the default event."

	^ self waitFor: aSymbolOrNil maxMilliseconds: nil