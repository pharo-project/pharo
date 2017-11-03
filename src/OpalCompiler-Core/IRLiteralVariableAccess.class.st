"
LiteralVariables are 

-> Globals
-> Pool access
-> Class Var access
"
Class {
	#name : #IRLiteralVariableAccess,
	#superclass : #IRAccess,
	#instVars : [
		'association'
	],
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #accessing }
IRLiteralVariableAccess >> association [ 
	^association
]

{ #category : #accessing }
IRLiteralVariableAccess >> association: anAssociation [
	association := anAssociation
]

{ #category : #testing }
IRLiteralVariableAccess >> isLiteralVariable [
	^true
]

{ #category : #accessing }
IRLiteralVariableAccess >> name [
	^association key
]
