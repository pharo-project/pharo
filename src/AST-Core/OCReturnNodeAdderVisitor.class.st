"
I am a visitor that wraps a node in a return node. If the node is sequence it will wrap the last statement in a return.
"
Class {
	#name : 'OCReturnNodeAdderVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitArrayNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitAssignmentNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitBlockNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitCascadeNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitLiteralNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitMessageNode: aNode [

	^ OCReturnNode value: aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitReturnNode: aNode [

	^ aNode
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitSequenceNode: aNode [

	"Add return returns the newly added return node, but nobody uses it"
	aNode addReturn.
	^ aNode 
]

{ #category : 'visiting' }
OCReturnNodeAdderVisitor >> visitVariableNode: aNode [

	^ OCReturnNode value: aNode
]
