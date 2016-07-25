visiting
visitSequence: instructionSequence
	prevInstr := nil.
	storePopToFix reset.
	retToFix reset.
	super visitSequence: instructionSequence.
	retToFix do: [ :each | self convertRet: each forSeq: instructionSequence ].
	storePopToFix do: [ :each | self convertStorePop: each forSeq: instructionSequence ]