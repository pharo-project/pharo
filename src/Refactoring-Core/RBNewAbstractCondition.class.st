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

	^ ' please redefine me '
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> not [
	^RBNewNegationCondition on: self
]

{ #category : 'logical operations' }
RBNewAbstractCondition >> | aCondition [
	"(A | B) = (A not & B not) not"

	^(self not & aCondition not) not
]
