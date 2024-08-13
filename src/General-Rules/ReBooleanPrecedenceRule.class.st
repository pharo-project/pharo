"
Checks precedence ordering of & and | with equality operators. Since | and & have the same precedence as =, there are common mistakes where parenthesis are missing around the equality operators.
"
Class {
	#name : 'ReBooleanPrecedenceRule',
	#superclass : 'ReNodeMatchRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReBooleanPrecedenceRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReBooleanPrecedenceRule class >> ruleName [
	^ 'Uses A | B = C instead of A | (B = C)'
]

{ #category : 'accessing' }
ReBooleanPrecedenceRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReBooleanPrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'BooleanPrecedenceRule'
]

{ #category : 'initialization' }
ReBooleanPrecedenceRule >> initialize [
	super initialize.
	self  matchesAny: #(
			'`@object1 | `@object2 = `@object3'
			'`@object1 | `@object2 == `@object3'
			'`@object1 & `@object2 = `@object3'
			'`@object1 & `@object2 == `@object3'
			'`@object1 | `@object2 ~= `@object3'
			'`@object1 | `@object2 ~~ `@object3'
			'`@object1 & `@object2 ~= `@object3'
			'`@object1 & `@object2 ~~ `@object3' )
]
