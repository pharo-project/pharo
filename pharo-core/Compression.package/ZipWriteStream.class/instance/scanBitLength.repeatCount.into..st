dynamic blocks
scanBitLength: bitLength repeatCount: repeatCount into: anArray
	"Update the frequency for the aTree based on the given values"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			anArray at: Repeat11To138+1 put: (anArray at: Repeat11To138+1) + 1.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			anArray at: Repeat3To10+1 put: (anArray at: Repeat3To10+1) + 1.
			count := (count - 10) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	] ifFalse:[
		anArray at: bitLength+1 put: (anArray at: bitLength+1) + 1.
		count := count - 1.
		[count >= 3] whileTrue:[
			anArray at: Repeat3To6+1 put: (anArray at: Repeat3To6+1) + 1.
			count := (count - 6) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	].