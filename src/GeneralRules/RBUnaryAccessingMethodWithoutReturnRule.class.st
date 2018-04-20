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
RBUnaryAccessingMethodWithoutReturnRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(aMethod numArgs > 0 or: [ aMethod isAbstract ]) ifTrue: [ ^ self ].
	((aMethod methodClass organization categoryOfElement: aMethod selector) asString
		beginsWith: 'accessing')
		ifFalse: [ ^ self ].
	aMethod ast
		nodesDo: [ :each | 
			each isReturn
				ifTrue: [ ^ self ] ].
	
	aCriticBlock cull:  (self critiqueFor: aMethod)
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBUnaryAccessingMethodWithoutReturnRule >> name [
	^ 'Unary "accessing" method without explicit return'
]
