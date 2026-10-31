"
I answer the maximum stack depth needed to evaluate an AST node, assuming dynamic arrays are compiled with pushConsArray.

Values pushed one after the other stay on the stack, so the depth at the Nth one is N - 1 + its own depth. Each visit method answers an integer.

Inlined messages are treated as normal sends, so the answer is an upper bound for them.
"
Class {
	#name : 'OCStackDepthVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#category : 'OpalCompiler-Core-Translator',
	#package : 'OpalCompiler-Core',
	#tag : 'Translator'
}

{ #category : 'visiting' }
OCStackDepthVisitor >> depthOfPushing: aCollectionOfNodes [

	| depth |
	depth := 0.
	aCollectionOfNodes withIndexDo: [ :node :index | depth := depth max: index - 1 + (self visitNode: node) ].
	^ depth
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitAnnotationMarkNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitArrayNode: anArrayNode [

	^ (self depthOfPushing: anArrayNode statements) max: 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitAssignmentNode: anAssignmentNode [

	^ self visitNode: anAssignmentNode value
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitBlockNode: aBlockNode [

	aBlockNode isInlined ifTrue: [ ^ self visitNode: aBlockNode body ].
	^ aBlockNode scope inComingCopiedVarNames size max: 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitCascadeNode: aCascadeNode [

	| depth |
	depth := self visitNode: aCascadeNode receiver.
	aCascadeNode messages do: [ :message | depth := depth max: 2 + (self depthOfPushing: message arguments) ].
	^ depth
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitEnglobingErrorNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitLiteralArrayNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitLiteralNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitLiteralValueNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitMessageNode: aMessageNode [

	aMessageNode isAnnotation ifTrue: [ ^ 1 ].
	^ self depthOfPushing: { aMessageNode receiver } , aMessageNode arguments
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitMethodNode: aMethodNode [

	^ self visitNode: aMethodNode body
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitParseErrorNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitReturnNode: aReturnNode [

	^ self visitNode: aReturnNode value
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitSequenceNode: aSequenceNode [

	^ aSequenceNode statements inject: 1 into: [ :depth :statement | depth max: (self visitNode: statement) ]
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitStoreIntoTempNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitStorePopIntoTempNode: aNode [

	^ 1
]

{ #category : 'visiting' }
OCStackDepthVisitor >> visitVariableNode: aVariableNode [

	^ 1
]
