"
Verifies that a given class directly defines a given set of selectors.
"
Class {
	#name : 'ReDefinesSelectorsCondition',
	#superclass : 'ReMethodsCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReDefinesSelectorsCondition >> definesSelectors: aSelectorsList in: aClass [

	class := aClass.
	selectors := aSelectorsList
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
	self violators ifNotEmpty: [ aStream nextPutAll: 'Selectors must be defined in the class.' ]
]

{ #category : 'displaying' }
ReDefinesSelectorsCondition >> violationMessageOn: aStream violators: theViolators [

	| verb1 verb2 |
	theViolators ifEmpty: [ ^ self ].
	theViolators = self violators
		ifTrue: [
				verb1 := ' is not'.
				verb2 := ' must' ]
		ifFalse: [
				verb1 := ' is'.
				verb2 := ' must not' ].
	theViolators do: [ :violator |
			aStream
				nextPutAll: violator;
				nextPutAll: verb1;
				nextPutAll: ' defined in the class ';
				nextPutAll: class name;
				nextPutAll: '.';
				space ].
	theViolators ifNotEmpty: [
			aStream
				nextPutAll: 'Selectors ';
				nextPutAll: verb2;
				nextPutAll: ' be defined in the class.' ]
]

{ #category : 'accessing' }
ReDefinesSelectorsCondition >> violators [

	^ violators ifNil: [
		  violators := self candidates reject: [ :aSelector | class directlyDefinesMethod: aSelector ] ]
]
