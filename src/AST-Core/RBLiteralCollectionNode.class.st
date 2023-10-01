"
I represent a literal collection as in `{ :Set 1.3.1}`.
My instances knows the collection class, here `Set` in addition to the state and behavior of my superclass `RBArrayNode`. 
"
Class {
	#name : 'RBLiteralCollectionNode',
	#superclass : 'RBArrayNode',
	#instVars : [
		'collectionClass'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'visiting' }
RBLiteralCollectionNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralCollectionNode: self
]

{ #category : 'accessing' }
RBLiteralCollectionNode >> collectionClass [

	^ collectionClass
]

{ #category : 'accessing' }
RBLiteralCollectionNode >> collectionClass: anObject [

	collectionClass := anObject
]
