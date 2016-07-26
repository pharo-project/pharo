private
generateSandQ
	"Generate a 160-bit random seed s and an industrial grade prime q."

	| hasher s sPlusOne u q |
	hasher := SHA1 new.
	[true] whileTrue: [
		s := self nextRandom160.
		sPlusOne := s + 1.
		sPlusOne highBit > 160 ifTrue: [sPlusOne := sPlusOne \\ (2 raisedTo: 160)].
		u := (hasher hashInteger: s) bitXor: (hasher hashInteger: sPlusOne).
		q := u bitOr: ((1 bitShift: 159) bitOr: 1).
		(self isProbablyPrime: q) ifTrue: [^ Array with: s with: q]].
