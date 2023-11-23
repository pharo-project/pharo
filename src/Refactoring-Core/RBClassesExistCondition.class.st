Class {
	#name : 'RBClassesExistCondition',
	#superclass : 'RBClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
RBClassesExistCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' does not exist.';
			space ]
]

{ #category : 'accessing' }
RBClassesExistCondition >> violators [

	^ violators ifNil: [
		  violators := OrderedCollection new.
		  classes keysAndValuesDo: [ :className :class |
			  class ifNil: [ violators add: className ] ].
		  violators ]
]
