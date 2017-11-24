"
I am cleaning the semantic analysis of the AST.

-> scopes of Blocks and Methpd
-> binding from Variables
"
Class {
	#name : #OCASTSemanticCleaner,
	#superclass : #RBProgramNodeVisitor,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #api }
OCASTSemanticCleaner class >> clean: aMethodNode [
	self new visitNode: aMethodNode.
	^aMethodNode
]

{ #category : #visiting }
OCASTSemanticCleaner >> visitBlockNode: aBlockNode [
	aBlockNode removeProperty: #scope ifAbsent: [ ].
	super visitBlockNode: aBlockNode.
	
]

{ #category : #visiting }
OCASTSemanticCleaner >> visitMethodNode: aMethodNode [
	aMethodNode removeProperty: #scope ifAbsent: [ ].
	super visitMethodNode: aMethodNode
]

{ #category : #visiting }
OCASTSemanticCleaner >> visitVariableNode: aVariableNode [
	aVariableNode removeProperty: #binding ifAbsent: [].
	aVariableNode primitiveChangeClassTo: RBVariableNode new.
]
