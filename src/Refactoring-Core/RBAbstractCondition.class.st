"
I am the abstract base class of refactoring conditions.
Conditions are used by refactoring operation for checking preconditions before applying the 
refactoring.

I implement some common behavior for error handling - errorMacro.
And to combine conditions with boolean operatiosn (and/or/not).

Most of the condition checking behavior is implemented on my subclass 
RBCondition. 

Instances of RBConditions are created by factory methods on its class side.
"
Class {
	#name : 'RBAbstractCondition',
	#superclass : 'Object',
	#instVars : [
		'errorMacro',
		'model'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'logical operations' }
RBAbstractCondition >> & aCondition [
	^RBAndCondition new left: self right: aCondition
]

{ #category : 'checking' }
RBAbstractCondition >> check [
	self subclassResponsibility
]

{ #category : 'private' }
RBAbstractCondition >> errorMacro [

	^ errorMacro ifNil: [ 'unknown' ] ifNotNil: [ errorMacro ]
]

{ #category : 'private' }
RBAbstractCondition >> errorMacro: aString [
	errorMacro := aString
]

{ #category : 'accessing' }
RBAbstractCondition >> errorString [
	^self errorStringFor: false
]

{ #category : 'private' }
RBAbstractCondition >> errorStringFor: aBoolean [
	^self errorMacro expandMacrosWith: aBoolean
]

{ #category : 'logical operations' }
RBAbstractCondition >> model [
	^ model ifNil: [ model := RBNamespace new ]
]

{ #category : 'accessing' }
RBAbstractCondition >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : 'logical operations' }
RBAbstractCondition >> not [
	^ReNewNegationCondition on: self
]

{ #category : 'logical operations' }
RBAbstractCondition >> | aCondition [
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not
]
