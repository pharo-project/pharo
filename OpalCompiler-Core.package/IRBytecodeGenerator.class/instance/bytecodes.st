results
bytecodes
	| stream |
	self updateJumpOffsets.
	stream := (ByteArray new: 100) writeStream. 
	primitiveBytes ifNotNil: [stream nextPutAll: primitiveBytes contents].
	orderSeq do: [ :seqId | |lastInstr|
			"If the instruction will emit bytes, record the first byte emitted as the instructions bytecode index"
			lastInstr := IRInstruction new -> 1.
			(instrMaps at: seqId) do: [ :assoc | |nextInstr|
				nextInstr := assoc.
				lastInstr key bytecodeIndex: ((nextInstr value - lastInstr value) > 0 ifTrue: [ 
					stream position + lastInstr value]).
				lastInstr := assoc.
				  ].
			lastInstr key bytecodeIndex: ((seqBytes at: seqId) size < lastInstr value ifFalse: [  stream position + lastInstr value]).
			stream nextPutAll: (seqBytes at: seqId) ].
	^ stream contents