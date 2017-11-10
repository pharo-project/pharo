"
Checks for any unary ""accessing"" methods without explicit return statements.
"
Class {
	#name : #RBUnaryAccessingMethodWithoutReturnRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
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
	(aMethod numArgs > 0 or: [ aMethod isAbstract ])
		ifTrue: [ ^ false ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString
		beginsWith: 'accessing')
		ifFalse: [ ^ false ].
	aMethod parseTree
		nodesDo: [ :each | 
			each isReturn
				ifTrue: [ ^ false ] ].
	^ true
]

{ #category : #running }
RBUnaryAccessingMethodWithoutReturnRule >> checkMethod: aMethod [
	(self basicCheck: aMethod)
		ifTrue: [ result addMethod: aMethod ]
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> name [
	^ 'Unary "accessing" method without explicit return'
]
