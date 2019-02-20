"
I check that packages constituing the Pharo Bootstrap do not depend on any other package.
"
Class {
	#name : #RBPharoBootstrapRule,
	#superclass : #RBArchitectureLintRule,
	#category : #'Refactoring-Critics-ArchitectureRules'
}

{ #category : #testing }
RBPharoBootstrapRule class >> isVisible [

	self flag: 'The functionality was moved to PharoBootstrapRule, but as this rule is used by Monkey, and Monkey is not using Renraku model yet, the rule is hidden'.
	^ false
]

{ #category : #testing }
RBPharoBootstrapRule class >> new [
	
	^ PharoBootstrapRule asRBRule 
]

{ #category : #accessing }
RBPharoBootstrapRule class >> uniqueIdentifierName [
	^ 'PharoBootstrapRul'
]
