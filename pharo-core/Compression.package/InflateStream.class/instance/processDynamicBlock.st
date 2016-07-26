inflating
processDynamicBlock
	| nLit nDist nLen codeLength lengthTable bits |
	nLit := (self nextBits: 5) + 257.
	nDist := (self nextBits: 5) + 1.
	nLen := (self nextBits: 4) + 4.
	codeLength := Array new: 19.
	codeLength atAllPut: 0.
	1 to: nLen do:[:i|
		bits := #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) at: i.
		codeLength at: bits+1 put: (self nextBits: 3).
	].
	lengthTable := self huffmanTableFrom: codeLength mappedBy: nil.
	"RFC 1951: In other words, all code lengths form a single sequence..."
	codeLength := self decodeDynamicTable: nLit+nDist from: lengthTable.
	litTable := self 
				huffmanTableFrom: (codeLength copyFrom: 1 to: nLit)
				mappedBy: self literalLengthMap.
	distTable := self 
				huffmanTableFrom: (codeLength copyFrom: nLit+1 to: codeLength size)
				mappedBy: self distanceMap.
	state := state bitOr: BlockProceedBit.
	self proceedDynamicBlock.