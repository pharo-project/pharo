"
Checks that all temporary variables are both read and written. If an temporary variable is only read, you can replace all of the reads with nil, since it couldn''t have been assigned a value. If the variable is only written, then we don''t need to store the result since we never use it.
"
Class {
	#name : #RBOnlyReadOrWrittenTemporaryRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #testing }
RBOnlyReadOrWrittenTemporaryRule class >> isVisible [

	self flag: 'The functionality was moved to TemporaryNeitherReadNorWrittenRule, but as this rule is used by Monkey, and Monkey is not using Renraku model yet, the rule is hidden'.
	^ false
]

{ #category : #'as yet unclassified' }
RBOnlyReadOrWrittenTemporaryRule class >> new [
	
	^ GRTemporaryNeitherReadNorWrittenRule asRBRule 
]

{ #category : #accessing }
RBOnlyReadOrWrittenTemporaryRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'OnlyReadOrWrittenTemporaryRule'
]
