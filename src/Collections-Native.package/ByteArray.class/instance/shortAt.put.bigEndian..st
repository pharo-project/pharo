platform independent access
shortAt: index put: value bigEndian: aBool
	"Store a 16 bit integer quantity starting from the given byte index"
	self unsignedShortAt: index put: (value bitAnd: 16r7FFF) - (value bitAnd: -16r8000) bigEndian: aBool.
	^value