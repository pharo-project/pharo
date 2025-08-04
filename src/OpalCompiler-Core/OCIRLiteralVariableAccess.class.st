"
I represent access to the following variables:

- Global variables
- Shared pools
- Shared variables
"
Class {
	#name : 'OCIRLiteralVariableAccess',
	#superclass : 'OCIRAccess',
	#instVars : [
		'association'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'accessing' }
OCIRLiteralVariableAccess >> association [
	^association
]

{ #category : 'accessing' }
OCIRLiteralVariableAccess >> association: anAssociation [
	association := anAssociation
]

{ #category : 'testing' }
OCIRLiteralVariableAccess >> isLiteralVariable [
	^true
]

{ #category : 'accessing' }
OCIRLiteralVariableAccess >> name [
	^association key
]
