"
Checks for literal arrays that contain the #, symbol. The user may have thought that it was a separator.
"
Class {
	#name : #RBLiteralArrayContainsCommaRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBLiteralArrayContainsCommaRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBLiteralArrayContainsCommaRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayContainsCommaRule'
]

{ #category : #running }
RBLiteralArrayContainsCommaRule >> basicCheck: aMethod [ 
	^ aMethod allLiterals anySatisfy: [ :each | self doesLiteralArrayContainComma: each ]
]

{ #category : #running }
RBLiteralArrayContainsCommaRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) 
		ifTrue: [ result addMethod: aMethod ]
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
