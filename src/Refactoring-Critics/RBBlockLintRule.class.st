"
I am a special kind of rule where the verification is specified using a block. The methods checkClass: and checkMethods are the hooks to specify such verifications.
"
Class {
	#name : #RBBlockLintRule,
	#superclass : #RBBasicLintRule,
	#category : 'Refactoring-Critics'
}

{ #category : #testing }
RBBlockLintRule class >> isVisible [
	^ self name ~= #RBBlockLintRule
]

{ #category : #accessing }
RBBlockLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'BlockLintRule'
]

{ #category : #accessing }
RBBlockLintRule >> resultClass [
	^ RBSelectorEnvironment
]
