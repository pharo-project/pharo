"
The direct use of the global variables World and ActiveWorld is incorrect. 
Use ""self currentWorld"" instead.
"
Class {
	#name : #ReUsesWorldGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUsesWorldGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UsesWorldGlobalRule'
]

{ #category : #running }
ReUsesWorldGlobalRule >> basicCheck: aNode [
	^ aNode isGlobalVariable and: [ #(#World #ActiveWorld) includes: aNode name ]
]

{ #category : #accessing }
ReUsesWorldGlobalRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReUsesWorldGlobalRule >> name [
	^ 'Uses World or ActiveWorld directly'
]

{ #category : #accessing }
ReUsesWorldGlobalRule >> severity [

	^ #warning
]
