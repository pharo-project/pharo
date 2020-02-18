Extension { #name : #Context }

{ #category : #'*OpalCompiler-Core' }
Context >> executedPC [
	"Experiment to use the same logic as #pcRangeContextIsActive: Deeper in the stack		
 	the pc was already advanced one bytecode, so we need to go back this one bytecode, which		
 	can consist of multiple bytes. But on IR, we record the *last* bytecode offset as the offset of		
 	the IR instruction, which means we can just go back one"	
	| executedPC |	
			
	executedPC := self isDead ifTrue: [ self endPC ] ifFalse: [pc].
	^ executedPC - 1.
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
	"Return the source node of the method or the block of this context"
	^ self sourceNodeExecuted enclosingMethodOrBlockNode
]

{ #category : #'*OpalCompiler-Core' }
Context >> sourceNodeExecuted [
	"When down in the stack, I return the node that executed"
	^ (method sourceNodeForPC: self executedPC) 
	"Uncomment the following once the pc->AST mapping is fixed"
	"^ (method sourceNodeForPC: (previousPc ifNil: [ self startpc ])) "
]
