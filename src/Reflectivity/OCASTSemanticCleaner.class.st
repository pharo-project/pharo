"
I am cleaning the semantic analysis of the AST, i.e., I remove the scope properties that the semantic analysis put.
I reset also the binding of the variables (from VariableNode to Variable hierarchy): VariableNodes see their variable status to unresolved. 
I'm only used by reflectivity and should be packaged there.

"
Class {
	#name : 'OCASTSemanticCleaner',
	#superclass : 'OCProgramNodeVisitor',
	#category : 'Reflectivity',
	#package : 'Reflectivity'
}

{ #category : 'api' }
OCASTSemanticCleaner class >> clean: aMethodNode [
	self new visitNode: aMethodNode.
	^aMethodNode
]

{ #category : 'visiting' }
OCASTSemanticCleaner >> visitBlockNode: aBlockNode [
	aBlockNode removeProperty: #scope ifAbsent: [ ].
	super visitBlockNode: aBlockNode
]

{ #category : 'visiting' }
OCASTSemanticCleaner >> visitMethodNode: aMethodNode [
	aMethodNode removeProperty: #scope ifAbsent: [ ].
	super visitMethodNode: aMethodNode
]

{ #category : 'visiting' }
OCASTSemanticCleaner >> visitVariableNode: aVariableNode [
	aVariableNode variable: OCUnresolvedVariable instance
]
