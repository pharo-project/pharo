platform independent access
doubleAt: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self unsignedLongAt: index put: w1 bigEndian: bool.
	self unsignedLongAt: index + 4 put: w2 bigEndian: bool.
	^ value