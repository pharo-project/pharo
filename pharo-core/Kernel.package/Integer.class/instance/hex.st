converting
hex
	"Print the receiver as hex, prefixed with 16r.  DO NOT CHANGE THIS!  The Cog VMMaker depends on this.
	 Consider using any of
		printStringHex
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringHex
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"
	^self storeStringBase: 16