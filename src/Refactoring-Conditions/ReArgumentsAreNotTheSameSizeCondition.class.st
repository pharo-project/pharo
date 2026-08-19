"
Verifies that the given selector arguments size and the given array of arguments size are equal to the new selector's array of arguments size.
"
Class {
	#name : 'ReArgumentsAreNotTheSameSizeCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'oldSelector',
		'newSelector',
		'newArgs'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'intialization' }
ReArgumentsAreNotTheSameSizeCondition >> oldSelector: aSelector newSelector: anOtherSelector newArgs: args [

	oldSelector := aSelector.
	newSelector := anOtherSelector.
	newArgs := args
]

{ #category : 'displaying' }
ReArgumentsAreNotTheSameSizeCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' don''t have the necessary arguments number to replace senders. ';
			space ].
]

{ #category : 'accessing' }
ReArgumentsAreNotTheSameSizeCondition >> violators [

	^ violators ifNil: [
			  violators := OrderedCollection new.
			  oldSelector numArgs < newSelector numArgs ifTrue: [
				  oldSelector numArgs + newArgs size = newSelector numArgs ifFalse: [ violators add: oldSelector ] ].
			  violators ]
]
