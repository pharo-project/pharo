"
Checks for any unary ""accessing"" methods without explicit return statements.
"
Class {
	#name : #RBUnaryAccessingMethodWithoutReturnRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBUnaryAccessingMethodWithoutReturnRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule class >> uniqueIdentifierName [

	^ 'UnaryAccessingMethodWithoutReturnRule'
]

{ #category : #running }
RBUnaryAccessingMethodWithoutReturnRule >> basicCheck: aMethod [
	(aMethod numArgs > 0 or: [ aMethod isAbstract ]) ifTrue: [ ^ false ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString beginsWith: 'accessing') ifFalse: [ ^ false ].
	aMethod ast nodesDo: [ :each | each isReturn ifTrue: [ ^ false ] ].

	^ true
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> name [
	^ 'Unary "accessing" method without explicit return'
]
