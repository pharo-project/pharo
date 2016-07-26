control flow instructions
interpret: seq1 then: seq2 distance: distance
	| hasPopped hasPopped2 jmpSz topValue stackSize |
	instructionStream willJustPop ifTrue: [ 
			topValue := simulatedStack removeLast.
			instructionStream nextInstruction ].
	stackSize := simulatedStack size.
	hasPopped := self interpretSeq: seq1 baseStackSize: stackSize distance: distance - topValue notNil asBit.
	jmpSz := jumpSize.
	jumpSize := 0.
	topValue ifNotNil: [:e | simulatedStack addLast: e ].
	hasPopped2 := self interpretSeq: seq2 baseStackSize: stackSize distance: jmpSz.
	^ hasPopped2 or: [hasPopped]