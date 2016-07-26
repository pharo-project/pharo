results
spurVMHeader: literalsSize
	^ (CompiledMethod headerFlagForEncoder: self encoderClass) +
		(self numArgs bitShift: 24) +
		( self numTemps bitShift: 18) +
		literalsSize +
		(self hasPrimitive asBit bitShift: 16)