"
I am a visitor that wraps a node in a return node. If the node is sequence it will wrap the last statement in a return.
"
Class {
	#name : 'RBReturnNodeAdderVisitor',
	#superclass : 'RBProgramNodeVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitArrayNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitAssignmentNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitBlockNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitCascadeNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitLiteralNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitMessageNode: aNode [

	^ RBReturnNode value: aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitReturnNode: aNode [

	^ aNode
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitSequenceNode: aNode [

	"Add return returns the newly added return node, but nobody uses it"
	aNode addReturn.
	^ aNode 
]

{ #category : 'visiting' }
RBReturnNodeAdderVisitor >> visitVariableNode: aNode [

	^ RBReturnNode value: aNode
]
