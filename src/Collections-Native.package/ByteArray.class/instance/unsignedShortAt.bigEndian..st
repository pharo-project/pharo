platform independent access
unsignedShortAt: index bigEndian: aBool
	"Return a 16 bit unsigned integer quantity starting from the given byte index"
	^aBool 
		ifTrue:[((self at: index) bitShift: 8) + (self at: index+1)]
		ifFalse:[((self at: index+1) bitShift: 8) + (self at: index)].