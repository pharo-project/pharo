platform independent access
doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new: 2. 
	bool
		ifTrue: [dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl