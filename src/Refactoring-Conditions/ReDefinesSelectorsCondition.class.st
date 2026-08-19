"
Verifies that a given class directly defines a given set of selectors.
"
Class {
	#name : 'ReDefinesSelectorsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'initialization' }
ReDefinesSelectorsCondition >> definesSelectors: aSelectorsList in: aClass [

	class := aClass.
	selectors := aSelectorsList
]

{ #category : 'accessing' }
ReDefinesSelectorsCondition >> nonViolators [

	^ selectors reject: [ :sel | violators contains: sel ]
]

{ #category : 'displaying' }
ReDefinesSelectorsCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is not defined in the class ';
			nextPutAll: class name;
			nextPutAll: '.';
			space ].
	aStream nextPutAll: 'Selectors must be defined in the class.'
]

{ #category : 'accessing' }
ReDefinesSelectorsCondition >> violators [

	^ violators ifNil: [
		  violators := selectors reject: [ :aSelector | class directlyDefinesMethod: aSelector ] ]
]
