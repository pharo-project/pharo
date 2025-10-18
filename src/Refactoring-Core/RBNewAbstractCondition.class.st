Class {
	#name : 'RBNewAbstractCondition',
	#superclass : 'Object',
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

{ #category : 'logical operations' }
RBNewAbstractCondition >> not [
	^ReNewNegationCondition on: self
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
