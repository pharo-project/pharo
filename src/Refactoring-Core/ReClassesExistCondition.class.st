Class {
	#name : 'ReClassesExistCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassesExistCondition >> subjects [
	
	"I must override subjects because violators are symbols instead of classes.
	To correctly compute `nonViolators`, subjects and violators must have matching types."
	^ classes keys
]

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
		  violators := violators asArray ]
]
