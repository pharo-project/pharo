bit manipulation
bitClear: aMask 
	"Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask."

	^ (self bitOr: aMask) - aMask