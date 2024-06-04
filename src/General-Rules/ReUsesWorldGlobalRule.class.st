"
The direct use of the global variables World and ActiveWorld is incorrect. 
Use ""self currentWorld"" instead.
"
Class {
	#name : 'ReUsesWorldGlobalRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReUsesWorldGlobalRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReUsesWorldGlobalRule class >> ruleName [
	^ 'Uses World or ActiveWorld directly'
]

{ #category : 'accessing' }
ReUsesWorldGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UsesWorldGlobalRule'
]

{ #category : 'running' }
ReUsesWorldGlobalRule >> basicCheck: aNode [
	^ aNode isGlobalVariable and: [ #(#World #ActiveWorld) includes: aNode name ]
]
