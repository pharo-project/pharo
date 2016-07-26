chunks
processSBITChunk
	| rBits gBits bBits aBits |
	colorType = 6 ifFalse:[^self].
	rBits := chunk at: 1.
	gBits := chunk at: 2.
	bBits := chunk at: 3.
	aBits := chunk at: 4.
	(rBits = 5 and:[gBits = 5 and:[bBits = 5 and:[aBits = 1]]]) ifTrue:[
		depth := 16.
	].