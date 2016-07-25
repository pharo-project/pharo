scanning
scanFor: scanBlock
	"Answer the index of the first bytecode for which scanBlock
	 answers true when supplied with that bytecode."

	| method end |
	method := self method.
	end := method endPC.
	[pc <= end] whileTrue: 
		[(scanBlock value: (method at: pc)) ifTrue: [^true].
		[self method encoderClass isExtension: (self method at: pc)] whileTrue: [pc := pc + 2].
		pc := self nextPc: (method at: pc)].
	^false