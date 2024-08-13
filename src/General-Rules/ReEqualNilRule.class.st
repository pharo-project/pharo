"
Replaces = nil and == nil by isNil, ~= nil and ~~ nil by notNil to make the code more readable.  
"
Class {
	#name : 'ReEqualNilRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReEqualNilRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReEqualNilRule class >> ruleName [
	^ '= nil -> isNil AND ~= nil -> isNotNil'
]

{ #category : 'accessing' }
ReEqualNilRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'EqualNilRule'
]

{ #category : 'initialization' }
ReEqualNilRule >> initialize [
	super initialize.
	self
		replace: '``@object = nil' with: '``@object isNil';
		replace: '``@object == nil' with: '``@object isNil';
		replace: '``@object ~= nil' with: '``@object isNotNil';
		replace: '``@object ~~ nil' with: '``@object isNotNil'
]
