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
Context >> sourceNode [
	"Return the source node of the method or the block of this context. 
	Note: we can not just use the startPC to find the node as this ignores optimized blocks.
	We instead get the executed PC and then go up to the sourceNode of the block or method. 
	For the topmost context, sourceNodeExecuted is off by one but that is not a problem as 
	we still end up getting the node we want (via the bytecode that created it)"

	^ self sourceNodeExecuted methodOrBlockNode
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNodeExecuted [
	"When down in the stack, I return the node that executed"
	^ (method sourceNodeForPC: self executedPC) 
	"Uncomment the following once the pc->AST mapping is fixed"
	"^ (method sourceNodeForPC: (previousPc ifNil: [ self startpc ])) "
]
