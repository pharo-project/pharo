"
This check is similar to the ""References an undeclared variable"" check, but it looks for variables that are not defined in the class or in the undeclared dictionary. You probably had to work hard to get your code in this state.
"
Class {
	#name : #RBVariableNotDefinedRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBVariableNotDefinedRule class >> isVisible [

	self flag: 'The functionality was moved to GRUndeclaredVariableRule, but as this rule is used by Monkey, and Monkey is not using Renraku model yet, the rule is hidden'.
	^ false
]

{ #category : #'instance creation' }
RBVariableNotDefinedRule class >> new [
	
	^ GRUndeclaredVariableRule asRBRule
]

{ #category : #accessing }
RBVariableNotDefinedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'VariableNotDefinedRule'
]
