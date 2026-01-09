Class {
	#name : 'RBNewAbstractCondition',
	#superclass : 'Object',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'logical operations' }
RBNewAbstractCondition >> & aCondition [
	^RBAndCondition new left: self right: aCondition
]

{ #category : 'accessing - private' }
RBNewAbstractCondition >> candidates [

	self subclassResponsibility
]

{ #category : 'checking' }
RBNewAbstractCondition >> check [

	^ self violators isEmpty
]

{ #category : 'private' }
RBNewAbstractCondition >> errorBlock [
	^ nil
]

{ #category : 'displaying' }
RBNewAbstractCondition >> errorString [ 

	^ String streamContents: [ :s | self violationMessageOn: s violators: self violators ]
]

{ #category : 'testing' }
RBNewAbstractCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'testing' }
RBNewAbstractCondition >> isTrue [

	^ self check
]

{ #category : 'accessing' }
RBNewAbstractCondition >> model [
	^ model ifNil: [ model := RBNamespace new ]
]

{ #category : 'accessing' }
RBNewAbstractCondition >> nonViolators [

	^ self candidates difference: self violators
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> not [
	^ReNewNegationCondition on: self
]

{ #category : 'displaying' }
RBNewAbstractCondition >> violationMessageOn: aStream [

	aStream nextPutAll: self errorString
]

{ #category : 'displaying' }
RBNewAbstractCondition >> violationMessageOn: aStream violators: theViolators [

	| isNegated |
	theViolators ifEmpty:[^self].
	isNegated := self violators ~= theViolators.
	
	theViolators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: '<1? is not:is>';
			nextPutAll: ' a valid package name.';
			space ]
]

{ #category : 'accessing' }
RBNewAbstractCondition >> violators [

	self subclassResponsibility
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> | aCondition [
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not
]
