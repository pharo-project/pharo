platform independent access
unsignedLongAt: index put: value bigEndian: aBool
	"Store a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 b3 |
	b0 := value bitShift: -24.
	b1 := (value bitShift: -16) bitAnd: 255.
	b2 := (value bitShift: -8) bitAnd: 255.
	b3 := value bitAnd: 255.
	aBool ifTrue:[
		self at: index put: b0.
		self at: index+1 put: b1.
		self at: index+2 put: b2.
		self at: index+3 put: b3.
	] ifFalse:[
		self at: index put: b3.
		self at: index+1 put: b2.
		self at: index+2 put: b1.
		self at: index+3 put: b0.
	].
	^value