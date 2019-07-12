"
The direct use of the global variables World and ActiveWorld is incorrect. 
Use ""self currentWorld"" instead.
"
Class {
	#name : #RBUsesWorldGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBUsesWorldGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UsesWorldGlobalRule'
]

{ #category : #running }
RBUsesWorldGlobalRule >> basicCheck: aNode [
	^ aNode isVariable and: [ aNode isGlobal and: [ #(#World #ActiveWorld) includes: aNode name ] ]
]

{ #category : #accessing }
RBUsesWorldGlobalRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBUsesWorldGlobalRule >> name [
	^ 'Uses World/ActiveWorld directly'
]

{ #category : #accessing }
RBUsesWorldGlobalRule >> severity [

	^ #warning
]
