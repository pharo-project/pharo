Class {
	#name : 'ReClassesExistCondition',
	#superclass : 'ReClassesCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'accessing' }
ReClassesExistCondition >> classes: aCollection [
	"I must override `classes:` because violators are symbols instead of classes.
	To correctly compute `nonViolators`, subjects and violators must have matching types."
	classes := aCollection.
	subjects := classes keys 
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
