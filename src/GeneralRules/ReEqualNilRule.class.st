"
Replaces = nil and == nil by isNil, ~= nil and ~~ nil by notNil to make the code more readable.  
"
Class {
	#name : #ReEqualNilRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReEqualNilRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EqualNilRule'
]

{ #category : #accessing }
ReEqualNilRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReEqualNilRule >> initialize [
	super initialize.
	self
		replace: '``@object = nil' with: '``@object isNil';
		replace: '``@object == nil' with: '``@object isNil';
		replace: '``@object ~= nil' with: '``@object notNil';
		replace: '``@object ~~ nil' with: '``@object notNil'
]

{ #category : #accessing }
ReEqualNilRule >> name [
	^ '= nil -> isNil AND ~= nil -> notNil'
]
