inflating
processFixedBlock
	litTable := self 
				huffmanTableFrom: FixedLitCodes
				mappedBy: self literalLengthMap.
	distTable := self 
				huffmanTableFrom: FixedDistCodes
				mappedBy: self distanceMap.
	state := state bitOr: BlockProceedBit.
	self proceedFixedBlock.