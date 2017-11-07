"
Replaces = nil and == nil by isNil, ~= nil and ~~ nil by notNil to make the code more readable.  
"
Class {
	#name : #RBEqualNilRule,
	#superclass : #ReNodeRewriteRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #accessing }
RBEqualNilRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EqualNilRule'
]

{ #category : #accessing }
RBEqualNilRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBEqualNilRule >> initialize [
	super initialize.
	self
		replace: '``@object = nil' with: '``@object isNil';
		replace: '``@object == nil' with: '``@object isNil';
		replace: '``@object ~= nil' with: '``@object notNil';
		replace: '``@object ~~ nil' with: '``@object notNil'
]

{ #category : #accessing }
RBEqualNilRule >> name [
	^ '= nil -> isNil AND ~= nil -> notNil'
]
