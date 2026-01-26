Extension { #name : 'CompiledBlock' }

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> ast [
	^ self sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> bcToASTCacheKey [
	"The bcToASTCache of a compiled block is saved in its home method's properties, with its pc in outer code as key.
	So this block is created at pc `48` in its outer code, which is its home method, then its key would be `#48`.
	If this block is created at pc `57` in its outer code, which is itself a block created at pc `48` in its outer code that is its home method, then the key of this block would be `#48>57`"

	| key outerCode |
	key := self pcInOuter asString.
	outerCode := self outerCode.

	outerCode isCompiledBlock ifTrue: [
		key := '{1}>{2}' format: {
				       outerCode pcInOuter.
				       key }.
		outerCode := outerCode outerCode ].
	^ key
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> methodNode [
	^ self outerCode methodNode
]

{ #category : '*OpalCompiler-Core' }
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
				((literal isKindOf: CleanBlockClosure) and: [literal compiledBlock == self  ])ifTrue: [ ^ instructionStream pc ].  ].
			instructionStream pc: (instructionStream nextPc: (outer at: instructionStream pc))].

	self error: 'block not installed in outer code'
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> sourceNode [
	^ self sourceNodeInOuter
]

{ #category : '*OpalCompiler-Core' }
CompiledBlock >> sourceNodeInOuter [
	^ self outerCode sourceNodeForPC: self pcInOuter
]
