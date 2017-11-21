"
Checks precedence ordering of & and | with equality operators. Since | and & have the same precedence as =, there are common mistakes where parenthesis are missing around the equality operators.
"
Class {
	#name : #RBBooleanPrecedenceRule,
	#superclass : #ReNodeMatchRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBBooleanPrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'BooleanPrecedenceRule'
]

{ #category : #accessing }
RBBooleanPrecedenceRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBBooleanPrecedenceRule >> initialize [
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

{ #category : #accessing }
RBBooleanPrecedenceRule >> name [
	^ 'Uses A | B = C instead of A | (B = C)'
]

{ #category : #accessing }
RBBooleanPrecedenceRule >> severity [
	^ #error
]
