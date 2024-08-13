"
Checks for any unary ""accessing"" methods without explicit return statements.
"
Class {
	#name : 'ReUnaryAccessingMethodWithoutReturnRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing' }
ReUnaryAccessingMethodWithoutReturnRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReUnaryAccessingMethodWithoutReturnRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReUnaryAccessingMethodWithoutReturnRule class >> ruleName [
	^ 'Unary "accessing" method without explicit return'
]

{ #category : 'accessing' }
ReUnaryAccessingMethodWithoutReturnRule class >> uniqueIdentifierName [

	^ 'UnaryAccessingMethodWithoutReturnRule'
]

{ #category : 'running' }
ReUnaryAccessingMethodWithoutReturnRule >> basicCheck: aMethod [

	(aMethod numArgs > 0 or: [ aMethod isAbstract ]) ifTrue: [ ^ false ].
	(aMethod protocolName beginsWith: 'accessing') ifFalse: [ ^ false ].
	^ aMethod ast allChildren noneSatisfy: [ :each | each isReturn ]
]
