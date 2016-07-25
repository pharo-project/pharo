accumulator ops
leftRotateBy: bits
	"Rotate my contents left by the given number of bits, retaining exactly 32 bits."
	"Details: Perform this operation with as little LargeInteger arithmetic as possible."

	| bitCount s1 s2 newHi |
	"ensure bitCount is in range [0..32]"
	bitCount := bits \\ 32.
	bitCount < 0 ifTrue: [bitCount := bitCount + 32].

	bitCount > 16
		ifTrue: [
			s1 := bitCount - 16.
			s2 := s1 - 16.
			newHi := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).
			low := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).
			hi := newHi]
		ifFalse: [
			s1 := bitCount.
			s2 := s1 - 16.
			newHi := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).
			low := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).
			hi := newHi]
