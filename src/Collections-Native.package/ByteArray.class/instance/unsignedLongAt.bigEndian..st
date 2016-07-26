platform independent access
unsignedLongAt: index bigEndian: aBool
	"Return a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 w |
	aBool ifTrue:[
		b0 := self at: index.
		b1 := self at: index+1.
		b2 := self at: index+2.
		w := self at: index+3.
	] ifFalse:[
		w := self at: index.
		b2 := self at: index+1.
		b1 := self at: index+2.
		b0 := self at: index+3.
	].
	"Minimize LargeInteger arithmetic"
	b2 = 0 ifFalse:[w := (b2 bitShift: 8) + w].
	b1 = 0 ifFalse:[w := (b1 bitShift: 16) + w].
	b0 = 0 ifFalse:[w := (b0 bitShift: 24) + w].
	^w