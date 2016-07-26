private
blockScopeRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1