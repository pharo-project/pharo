private
scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner
	| bsl maybeBlockSize |
	bsl := BlockStartLocator new.
	scanner pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[refpc = scanner pc ifTrue:
			[scanner pc: startpc.
			 [scanner pc <= endpc] whileTrue:
				[(scan value: scanner firstByte) ifTrue:
					[^endpc].
				 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
					[scanner pc: scanner pc + maybeBlockSize]].
			   ^self].
		 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
			[refpc <= (scanner pc + maybeBlockSize)
				ifTrue: [^self scanBlockScopeFor: refpc from: scanner pc to: scanner pc + maybeBlockSize with: scan scanner: scanner]
				ifFalse: [scanner pc: scanner pc + maybeBlockSize]]]