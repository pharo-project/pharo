dynamic blocks
sendBitLengthTree: blTree
	"Send the bit length tree"
	| blIndex bitLength |
	MaxBitLengthCodes to: 4 by: -1 do:[:maxIndex|
		blIndex := BitLengthOrder at: maxIndex.
		bitLength := blIndex <= blTree maxCode 
			ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
		(maxIndex = 4 or:[bitLength > 0]) ifTrue:[
			encoder nextBits: 4 put: maxIndex - 4.
			1 to: maxIndex do:[:j|
				blIndex := BitLengthOrder at: j.
				bitLength := blIndex <= blTree maxCode 
					ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
				encoder nextBits: 3 put: bitLength].
			^self]].