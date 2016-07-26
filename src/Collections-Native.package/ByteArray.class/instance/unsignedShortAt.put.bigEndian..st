platform independent access
unsignedShortAt: index put: value bigEndian: aBool
	"Store a 16 bit unsigned integer quantity starting from the given byte index"
	aBool ifTrue:[
		self at: index put: (value bitShift: -8).
		self at: index+1 put: (value bitAnd: 255).
	] ifFalse:[
		self at: index+1 put: (value bitShift: -8).
		self at: index put: (value bitAnd: 255).
	].
	^value