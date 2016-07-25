dynamic blocks
sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits
	"Send all the trees needed for dynamic huffman tree encoding"
	| lastValue lastCount value |
	encoder nextBits: 5 put: (lTree maxCode - 256).
	encoder nextBits: 5 put: (dTree maxCode).
	self sendBitLengthTree: blTree.
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
					lastValue := value.
					lastCount := 1]].
	self sendBitLength: lastValue repeatCount: lastCount tree: blTree.