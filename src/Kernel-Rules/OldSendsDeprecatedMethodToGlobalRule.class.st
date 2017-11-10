"
Checks for sends of deprecated messages that might be removed in upcoming releases of Pharo.
"
Class {
	#name : #OldSendsDeprecatedMethodToGlobalRule,
	#superclass : #RBBlockLintRule,
	#category : #Kernel-Rules
}

{ #category : #'as yet unclassified' }
OldSendsDeprecatedMethodToGlobalRule class >> checksMethod [

	^ true
]

{ #category : #accessing }
OldSendsDeprecatedMethodToGlobalRule class >> isVisible [

	self flag: 'The functionality was moved to SendsDeprecatedMethodToGlobalRule, but as this rule is used by Monkey, and Monkey is not using Renraku model yet, the rule is hidden'.
	^ false
]

{ #category : #'instance creation' }
OldSendsDeprecatedMethodToGlobalRule class >> new [
	
	^ SendsDeprecatedMethodToGlobalRule asRBRule
]

{ #category : #accessing }
OldSendsDeprecatedMethodToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsDeprecatedMethodToGlobalRule'
]

{ #category : #accessing }
OldSendsDeprecatedMethodToGlobalRule >> name [
	^ 'Sends a deprecated message to a known global'
]
