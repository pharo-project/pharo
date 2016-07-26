converting
asUnsignedLong "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as an unsigned value."
	
	^self unsignedLongAt: 1