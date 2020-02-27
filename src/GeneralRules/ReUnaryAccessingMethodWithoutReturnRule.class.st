"
Checks for any unary ""accessing"" methods without explicit return statements.
"
Class {
	#name : #ReUnaryAccessingMethodWithoutReturnRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUnaryAccessingMethodWithoutReturnRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUnaryAccessingMethodWithoutReturnRule class >> uniqueIdentifierName [

	^ 'UnaryAccessingMethodWithoutReturnRule'
]

{ #category : #running }
ReUnaryAccessingMethodWithoutReturnRule >> basicCheck: aMethod [
	(aMethod numArgs > 0 or: [ aMethod isAbstract ]) ifTrue: [ ^ false ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString beginsWith: 'accessing') ifFalse: [ ^ false ].
	aMethod ast nodesDo: [ :each | each isReturn ifTrue: [ ^ false ] ].

	^ true
]

{ #category : #accessing }
ReUnaryAccessingMethodWithoutReturnRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReUnaryAccessingMethodWithoutReturnRule >> name [
	^ 'Unary "accessing" method without explicit return'
]
