Extension { #name : 'Context' }

{ #category : '*OpalCompiler-Core' }
Context >> astScope [
	"Return the scope of the method or the block of this context.
	Note: if the compiler would not model scopes for optimized blocks, this would just just be the
	scope of the compiledCode ast.
	We instead use sourceNodeExecuted and then get the the block or methodnode on the AST level.
	For the topmost context, sourceNodeExecuted is off by one but that is not a problem as
	we still end up getting the scope we want"

	| node |
	node :=  self sourceNodeExecuted.
	"if we get back a block, this is a block that just got pushed. We are not interested in
	that but it's outer block"
	^node isBlock
		ifTrue: [ node parent methodOrBlockNode scope ]
		ifFalse: [node methodOrBlockNode scope ]
]

{ #category : '*OpalCompiler-Core' }
Context >> compiler [
	"Return a compiler set up to parse/compile/evaluate scripts on the current context.
	The compiler is also setup according to the class of the receiver"

	^ self receiver class compiler context: self
]

{ #category : '*OpalCompiler-Core' }
Context >> executedPC [
	"Deeper in the stack the pc was already advanced one bytecode, so we need to go back
	this one bytecode, which can consist of multiple bytes. But on IR, we record the *last*
	bytecode offset as the offset of the IR instruction, which means we can just go back one"

	"if we are at the start, return the startpc"
	(pc == self startpc) ifTrue: [ ^self startpc ].

	"we need to guard for dead contexts (pc is nil)"
	^self isDead ifTrue: [ self endPC - 1] ifFalse: [ pc - 1]
]

{ #category : '*OpalCompiler-Core' }
Context >> lookupVar: aSymbol [
	^ self astScope lookupVar: aSymbol
]

{ #category : '*OpalCompiler-Core' }
Context >> sourceNode [
	"Return the source node that created the method or the block of this context"

	^ closureOrNil
		ifNil: [ self compiledCode sourceNode ]
		ifNotNil: [ closureOrNil sourceNode ]
]

{ #category : '*OpalCompiler-Core' }
Context >> sourceNodeExecuted [
	"When down in the stack, I return the node that executed"
	^ method sourceNodeForPC: self executedPC
]
