Extension { #name : #Context }

{ #category : #'*OpalCompiler-Core' }
Context >> executedPC [
	"Deeper in the stack the pc was already advanced one bytecode, so we need to go back 
	this one bytecode, which can consist of multiple bytes. But on IR, we record the *last* 
	bytecode offset as the offset of the IR instruction, which means we can just go back one"		
			
	"if we are at the start, return the startpc"		
	(pc == self startpc) ifTrue: [ ^self startpc ].
	
	"we need to guard for dead contexts (pc is nil)"
	^self isDead ifTrue: [ self endPC - 1] ifFalse: [ pc - 1].
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
