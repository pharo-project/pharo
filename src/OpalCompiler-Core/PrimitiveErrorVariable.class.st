"
Primitives can declare a local variable that will hold the error code.
"
Class {
	#name : #PrimitiveErrorVariable,
	#superclass : #LocalVariable,
	#instVars : [
		#node
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #'instance creation' }
PrimitiveErrorVariable class >> node: aVariableNode [
	^self new
		name: aVariableNode name;
		node: aVariableNode;
		markWrite;
		yourself
]

{ #category : #visiting }
PrimitiveErrorVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitTemporaryVariableNode: aNode
]

{ #category : #queries }
PrimitiveErrorVariable >> definingNode [
	^node
]

{ #category : #accessing }
PrimitiveErrorVariable >> node: anObject [
	node := anObject
]
