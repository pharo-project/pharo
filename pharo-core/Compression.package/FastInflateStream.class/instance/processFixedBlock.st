inflating
processFixedBlock
	litTable := FixedLitTable.
	distTable := FixedDistTable.
	state := state bitOr: BlockProceedBit.
	self proceedFixedBlock.