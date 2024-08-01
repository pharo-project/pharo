Class {
	#name : 'ReClassesExistCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'displaying' }
ReClassesExistCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' does not exist.';
			space ]
]

{ #category : 'accessing' }
ReClassesExistCondition >> violators [

	^ violators ifNil: [
		  violators := OrderedCollection new.
		  classes keysAndValuesDo: [ :className :class |
			  class ifNil: [ violators add: className ] ].
		  violators ]
]
