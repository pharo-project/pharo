"
I represent an architectural rule.
I work at the package level and check dependencies among packages.
"
Class {
	#name : #RBArchitectureLintRule,
	#superclass : #RBBasicLintRule,
	#category : 'Refactoring-Critics-ArchitectureRules'
}

{ #category : #testing }
RBArchitectureLintRule class >> isVisible [
	^ self name ~= #RBArchitectureLintRule
]

{ #category : #accessing }
RBArchitectureLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	^ 'ArchitectureLintRule'
]

{ #category : #accessing }
RBArchitectureLintRule >> group [
	^ 'Architecture'
]

{ #category : #testing }
RBArchitectureLintRule >> isArchitecturalRule [
	^ true 
]

{ #category : #accessing }
RBArchitectureLintRule >> resultClass [
	^ RBPackageEnvironment
]
