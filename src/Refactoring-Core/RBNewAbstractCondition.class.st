Class {
	#name : 'RBNewAbstractCondition',
	#superclass : 'Object',
	#instVars : [
		'wrapper'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'logical operations' }
RBNewAbstractCondition >> & aCondition [
	^RBAndCondition new left: self right: aCondition
]

{ #category : 'checking' }
RBNewAbstractCondition >> check [
	self subclassResponsibility
]

{ #category : 'accessing' }
RBNewAbstractCondition >> errorString [ 

	^ String streamContents: [ :stream | self violationMessageOn: stream ] 
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
RBNewAbstractCondition >> nonViolators [ 
	self subclassResponsibility 
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> not [
	^ReNewNegationCondition on: self
]

{ #category : 'displaying' }
RBNewAbstractCondition >> violationMessageOn: aStream [
	self subclassResponsibility 
]

{ #category : 'accessing' }
RBNewAbstractCondition >> violators [
	self subclassResponsibility 
]

{ #category : 'accessing' }
RBNewAbstractCondition >> wrapper [
	^ wrapper ifNil: [ self ]
]

{ #category : 'accessing' }
RBNewAbstractCondition >> wrapper: aCondition [ 
	"A wrapper is a NegationCondition I am part of.
	Wrappers are always nil, except during the execution of `violationMessageOn:`.
	Without wrappers, violation messages are empty for negated conditions (because violators are empty too!)"

	wrapper := aCondition 
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> | aCondition [
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not
]
