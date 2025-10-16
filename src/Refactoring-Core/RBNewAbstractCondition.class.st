Class {
	#name : 'RBNewAbstractCondition',
	#superclass : 'Object',
	#instVars : [
		'subjects'
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

{ #category : 'logical operations' }
RBNewAbstractCondition >> not [
	^ReNewNegationCondition on: self
]

{ #category : 'accessing - private' }
RBNewAbstractCondition >> subjects [
	"Complex conditions require this method to compute their own subjects"
	^ subjects
]

{ #category : 'displaying' }
RBNewAbstractCondition >> violationMessageOn: aStream [
	self subclassResponsibility 
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> | aCondition [
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not
]
