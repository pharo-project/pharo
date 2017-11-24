"
This smell arises when an instance variable is not both read and written. If an instance variable is only read, the reads can be replaced by nil, since it could not have been assigned a value. If the variable is only written, then it does not need to store the result since it is never used. This check does not work for the data model classes since they use the #instVarAt:put: messages to set instance variables.
"
Class {
	#name : #RBOnlyReadOrWrittenVariableRule,
	#superclass : #RBBlockLintRule,
	#category : #'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBOnlyReadOrWrittenVariableRule class >> isVisible [

	self flag: 'The functionality was moved to GRIvarNeitherReadNorWrittenRule, but as this rule is used by Monkey, and Monkey is not using Renraku model yet, the rule is hidden'.
	^ false
]

{ #category : #'instance creation' }
RBOnlyReadOrWrittenVariableRule class >> new [
	
	^ GRIvarNeitherReadNorWrittenRule asRBRule 
]

{ #category : #accessing }
RBOnlyReadOrWrittenVariableRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'OnlyReadOrWrittenVariableRule'
]
