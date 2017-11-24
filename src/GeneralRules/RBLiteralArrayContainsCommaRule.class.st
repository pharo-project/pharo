"
Checks for literal arrays that contain the #, symbol. The user may have thought that it was a separator.
"
Class {
	#name : #RBLiteralArrayContainsCommaRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBLiteralArrayContainsCommaRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayContainsCommaRule'
]

{ #category : #running }
RBLiteralArrayContainsCommaRule >> check: aNode forCritiquesDo: aCritiqueBlock [ 
	aNode isLiteralArray ifFalse: [ ^ self ].
	(aNode value includes: #,) ifFalse: [ ^ self ].


	aCritiqueBlock cull: (self critiqueFor: aNode)
]

{ #category : #private }
RBLiteralArrayContainsCommaRule >> doesLiteralArrayContainComma: aLiteral [ 
	aLiteral class = Array 
		ifFalse: [ ^ false ].
	(aLiteral includes: #,)
		ifTrue: [ ^ true ].
	^ aLiteral 
		anySatisfy: [ :each | self doesLiteralArrayContainComma: each ]
]

{ #category : #accessing }
RBLiteralArrayContainsCommaRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBLiteralArrayContainsCommaRule >> name [
	^ 'Literal array contains a #,'
]
