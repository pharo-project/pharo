Class {
	#name : 'ReReifiedCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'wrapper'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReReifiedCondition >> errorString [

	^ String streamContents: [ :aStream | self violationMessageOn: aStream ]
]

{ #category : 'checking' }
ReReifiedCondition >> isFalse [

	^ self isTrue not
]

{ #category : 'checking' }
ReReifiedCondition >> isTrue [ 

	^ self check
]

{ #category : 'logical operations' }
ReReifiedCondition >> not [ 

	^ ReNewNegationCondition on: self
]

{ #category : 'displaying' }
ReReifiedCondition >> violationMessageOn: aStream [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ReReifiedCondition >> violators [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReReifiedCondition >> wrapper [
	^ wrapper ifNil: [ self ]
]

{ #category : 'accessing' }
ReReifiedCondition >> wrapper: aCondition [
	"A wrapper is a NegationCondition I am part of.
	Wrappers are always nil, except during the execution of `violationMessageOn:`.
	Without wrappers, violation messages are empty for negated conditions (because violators are empty too!)"

	wrapper := aCondition	
]
