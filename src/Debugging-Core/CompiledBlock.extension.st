Extension { #name : #CompiledBlock }

{ #category : #'*Debugging-Core' }
CompiledBlock >> pcInOuter [
	| outer end instructionStream |
	outer := self outerCode.
	instructionStream := InstructionStream on: outer.
	end := outer endPC.
	[instructionStream pc <= end] whileTrue: 
		[ (self encoderClass isCreateFullBlock: self code: outer at: instructionStream pc)
			ifTrue: [ ^ instructionStream pc ].
			instructionStream pc: (instructionStream nextPc: (outer at: instructionStream pc))].
	"scan for clean block"	
	instructionStream := InstructionStream on: outer.
	[instructionStream pc <= end] whileTrue: [
			| literalOffset |
			literalOffset := self encoderClass literalIndexOfBytecodeAt: instructionStream pc in: outer.
			literalOffset ifNotNil: [
				| literal |
				literal := (self outerCode literalAt: literalOffset + 1).
				(literal class == CleanBlockClosure and: [literal compiledBlock = self  ])ifTrue: [ ^ instructionStream pc ].  ].
			instructionStream pc: (instructionStream nextPc: (outer at: instructionStream pc))].	
		
	self error: 'block not installed in outer code'.
]
