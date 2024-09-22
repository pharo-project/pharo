"
I am a visitor that wraps a node in a return node. If the node is sequence it will wrap the last statement in a return.
"
Class {
	#name : 'ASTReturnNodeAdderVisitor',
	#superclass : 'ASTProgramNodeVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitArrayNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitAssignmentNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitBlockNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitCascadeNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitLiteralNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitMessageNode: aNode [

	^ ASTReturnNode value: aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitReturnNode: aNode [

	^ aNode
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitSequenceNode: aNode [

	"Add return returns the newly added return node, but nobody uses it"
	aNode addReturn.
	^ aNode 
]

{ #category : 'visiting' }
ASTReturnNodeAdderVisitor >> visitVariableNode: aNode [

	^ ASTReturnNode value: aNode
]
