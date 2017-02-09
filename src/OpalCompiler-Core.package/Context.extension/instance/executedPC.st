*OpalCompiler-Core
executedPC
	"If this context is already perform message send and waiting return 
	then executed PC is previous one. So we start analazing with previous PC.
	If there was no sends yet then we just need current pc corrected by push operations.
	Also we should skip block returns which surround following send"
	| executedPC |
	self isDead ifTrue: [ ^self startpc ].
	executedPC := self previousPc ifNil: [self startpc].
	
	(self isReturnAt: executedPC) ifTrue: [ executedPC := executedPC + 1 ].	
	"There is a pushLiteral: nil bytecode for each temps in a block. 
	There is a 'pushTemp:' bytecode for each copied value of a block.
	These bytecodes are not mapped to any IR.
	We skip both"
	[ self isPushLiteralNil: executedPC ] whileTrue: [ executedPC := executedPC + 1 ].
	[ self isPushTemp: executedPC ] whileTrue: [ executedPC := executedPC + 1 ].

	(self  isReturnAt: executedPC) ifTrue: [ executedPC := executedPC + 1 ].
	^ executedPC