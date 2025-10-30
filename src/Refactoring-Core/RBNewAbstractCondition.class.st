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

{ #category : 'accessing - private' }
RBNewAbstractCondition >> candidates [

	self subclassResponsibility
]

{ #category : 'checking' }
RBNewAbstractCondition >> check [

	^ self violators isEmpty
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
RBNewAbstractCondition >> violationMessageOn: aStream violators: aCollection [

	self subclassResponsibility
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
