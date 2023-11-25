Class {
	#name : 'RBDefinesSelectorsCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'class',
		'selectors',
		'violators'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBDefinesSelectorsCondition >> check [ 

	^ self violators isEmpty
]

{ #category : 'initialization' }
RBDefinesSelectorsCondition >> definesSelectors: aSelectorsList in: aClass [

	class := aClass.
	selectors := aSelectorsList
]

{ #category : 'displaying' }
RBDefinesSelectorsCondition >> violationMessageOn: aStream [

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
RBDefinesSelectorsCondition >> violators [

	^ violators ifNil: [
		  violators := selectors reject: [ :aSelector | class directlyDefinesMethod: aSelector ] ]
]
