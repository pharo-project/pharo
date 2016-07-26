endian
calcEndianness
	"store the least significant word of the float with 1; on big endian machines this is the second word"
	| f |
	f := Float new: 2.
	f at: 2 put: 1.
	self adoptInstance: f.
	^ (f at: 1) = 1 ifTrue: [#little] ifFalse: [#big]