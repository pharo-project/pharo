Class {
	#name : 'ReClassesExistCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReClassesExistCondition >> candidates [
	"I must override `candidates` because my violators are symbols instead of classes.
	To correctly compute `nonViolators`, candidates and violators must have matching types."
	^ classes keys
	
]

{ #category : 'displaying' }
ReClassesExistCondition >> violationMessageOn: aStream [
	self deprecated: 'Use violationMessageOn:violators: instead'.
	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' does not exist.';
			space ]
]

{ #category : 'displaying' }
ReClassesExistCondition >> violationMessageOn: aStream violators: theViolators [
	| verb |
	theViolators ifEmpty: [ ^self ].
	verb := theViolators = self violators ifTrue: [ ' does not exist' ] ifFalse: [ ' exists' ].
	theViolators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: verb;
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
