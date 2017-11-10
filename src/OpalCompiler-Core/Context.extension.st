Extension { #name : #Context }

{ #category : #'*OpalCompiler-Core' }
Context >> executedPC [
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
]

{ #category : #'*OpalCompiler-Core' }
Context >> isBlockReturn: aPC [
	^method encoderClass isBlockReturnAt: aPC in: method.
]

{ #category : #'*OpalCompiler-Core' }
Context >> isPushLiteralNil: aPC [
	^ (self method at: aPC) = method encoderClass pushNilByte
]

{ #category : #'*OpalCompiler-Core' }
Context >> isPushTemp: aPC [
	^ self method encoderClass isPushTempAt: aPC in: method
]

{ #category : #'*OpalCompiler-Core' }
Context >> isReturnAt: aPC [
	^method encoderClass isReturnAt: aPC in: method.
]

{ #category : #'*OpalCompiler-Core' }
Context >> neighborPCWithCorrectMapping [
	"Answer a pc inside the enclosing block or mathod that is correctly mapped to an AST node"
	"This is an ugly and temporary fix for Pharo 3. 
	Must be removed as soon as mappings are fixed"
	| neighborPC |
	neighborPC := self isDead 
		ifTrue: [ self startpc ]
		ifFalse: [ pc ].
	"There is a pushLiteral: nil bytecode for each temps in a block. 
	There is a 'pushTemp:' bytecode for each copied value of a block.
	These bytecodes are not mapped to any IR.
	We skip both"
	[ self isPushLiteralNil: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	[ self isPushTemp: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	"The block return ir instruction is mapped to the block instead of the sequence AST node. So we go just before it"
	(self isBlockReturn: neighborPC) ifTrue: [ neighborPC := neighborPC - 1 ].
	^ neighborPC
]

{ #category : #'*OpalCompiler-Core' }
Context >> previousPcWithCorrectMapping [
	"Answer a pc inside the enclosing block or mathod that is correctly mapped to an AST node"
	"This is an ugly and temporary fix for Pharo 3. 
	Must be removed as soon as mappings are fixed"
	| neighborPC |
	neighborPC := self isDead 
		ifTrue: [ self startpc ]
		ifFalse: [ self previousPc ifNil: [self startpc]].
	"There is a pushLiteral: nil bytecode for each temps in a block. 
	There is a 'pushTemp:' bytecode for each copied value of a block.
	These bytecodes are not mapped to any IR.
	We skip both"
	[ self isPushLiteralNil: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	[ self isPushTemp: neighborPC ] whileTrue: [ neighborPC := neighborPC + 1 ].
	"The block return ir instruction is mapped to the block instead of the sequence AST node. So we go just before it"
	(self isBlockReturn: neighborPC) ifTrue: [ neighborPC := neighborPC - 1 ].
	^ neighborPC
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNode [
	"Return the source node of the method or the block corresponding to the receiver"
	^ (method sourceNodeForPC: self neighborPCWithCorrectMapping) enclosingMethodOrBlockNode
	"Uncomment the following once the pc->AST mapping is fixed"
	"^ (method sourceNodeForPC: (pc ifNil: [ self startpc ])) enclosingMethodOrBlockNode"
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNodeExecuted [
	"When down in the stack, I return the node that executed"
	^ (method sourceNodeExecutedForPC: self executedPC) 
	"Uncomment the following once the pc->AST mapping is fixed"
	"^ (method sourceNodeForPC: (previousPc ifNil: [ self startpc ])) "
]
