"
Checks for literal arrays that contain the #, symbol. The user may have thought that it was a separator.
"
Class {
	#name : #ReLiteralArrayContainsCommaRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReLiteralArrayContainsCommaRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayContainsCommaRule'
]

{ #category : #running }
ReLiteralArrayContainsCommaRule >> basicCheck: aNode [
	^ aNode isLiteralArray and: [ aNode value includes: #, ]
]

{ #category : #private }
ReLiteralArrayContainsCommaRule >> doesLiteralArrayContainComma: aLiteral [ 
	aLiteral class = Array 
		ifFalse: [ ^ false ].
	(aLiteral includes: #,)
		ifTrue: [ ^ true ].
	^ aLiteral 
		anySatisfy: [ :each | self doesLiteralArrayContainComma: each ]
]

{ #category : #accessing }
ReLiteralArrayContainsCommaRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReLiteralArrayContainsCommaRule >> name [
	^ 'Literal array contains a #,'
]
