"
Pharo code should not contain linefeed characters.
"
Class {
	#name : #RBMethodSourceContainsLinefeedsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBMethodSourceContainsLinefeedsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBMethodSourceContainsLinefeedsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MethodSourceContainsLinefeedsRule'
]

{ #category : #running }
RBMethodSourceContainsLinefeedsRule >> basicCheck: aMethod [
	^ aMethod sourceCode includes: Character lf
]

{ #category : #accessing }
RBMethodSourceContainsLinefeedsRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBMethodSourceContainsLinefeedsRule >> name [
	^ 'Method source contains linefeeds'
]

{ #category : #accessing }
RBMethodSourceContainsLinefeedsRule >> severity [
	^ #error
]
