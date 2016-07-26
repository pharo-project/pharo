dynamic blocks
sendBitLength: bitLength repeatCount: repeatCount tree: aTree
	"Send the given bitLength, repeating repeatCount times"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			self sendBitLength: Repeat11To138 tree: aTree.
			encoder nextBits: 7 put: (count min: 138) - 11.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To10 tree: aTree.
			encoder nextBits: 3 put: (count min: 10) - 3.
			count := (count - 10) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	] ifFalse:[
		self sendBitLength: bitLength tree: aTree.
		count := count - 1.
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To6 tree: aTree.
			encoder nextBits: 2 put: (count min: 6) - 3.
			count := (count - 6) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	].