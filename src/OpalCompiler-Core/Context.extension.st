Extension { #name : #Context }

{ #category : #'*OpalCompiler-Core' }
Context >> astScope [
	"Return the scope of the method or the block of this context. 
	Note: we can not just use #sourceNode as this ignores optimized blocks.
	We instead use sourceNodeExecuted and then get the the block or methodnode on the AST level.
	For the topmost context, sourceNodeExecuted is off by one but that is not a problem as 
	we still end up getting the scope  we want"

	^ self sourceNodeExecuted methodOrBlockNode scope
]

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
	^ (self compiledCode at: aPC) = method encoderClass pushNilByte
]

{ #category : #'*OpalCompiler-Core' }
Context >> isPushTemp: aPC [
	^ self compiledCode encoderClass isPushTempAt: aPC in: method
]

{ #category : #'*OpalCompiler-Core' }
Context >> isReturnAt: aPC [
	^method encoderClass isReturnAt: aPC in: method.
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNode [
	"Return the source node that created the method or the block of this context"

	^ closureOrNil
		ifNil: [ self compiledCode sourceNode ]
		ifNotNil: [ closureOrNil sourceNode ]
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNodeExecuted [
	"When down in the stack, I return the node that executed"
	^ method sourceNodeForPC: self executedPC
]
