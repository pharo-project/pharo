"
Visit any RBCommentNode in the syntax tree and evaluate a generic block.

The block  has two optional parameters:  
	aCommentArray (the collection of 1 or more RBComment's)
	aNode (the node which contains the comment)
"
Class {
	#name : 'ASTCommentNodeVisitor',
	#superclass : 'ASTAbstractBlockVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'class initialization' }
ASTCommentNodeVisitor class >> initialize [

	self deprecatedAliases: { #ASTCommentNodeVisitor }
]

{ #category : 'visiting' }
ASTCommentNodeVisitor >> visitNode: aNode [
	aNode comments ifNotEmpty: [ :commentNodes |
		commentNodes do: [ :commentNode |
			self visitBlock cull: commentNode cull: aNode ] ].

	super visitNode: aNode
]
