"
I check if `name` is not already used by a variable in the `class` hierarchy
"
Class {
	#name : 'ReIsVariableNotDefinedInHierarchy',
	#superclass : 'ReVariableNameCondition',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReIsVariableNotDefinedInHierarchy class >> name: aString class: aClass [
	^ (self name: aString)
		class: aClass;
		yourself
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> class: aClass [
	class := aClass
]

{ #category : 'displaying' }
ReIsVariableNotDefinedInHierarchy >> violationMessageOn: aStream [
	self deprecated: 'User violationMessageOn:violators instead'. 
	^ aStream
		  nextPutAll: violator;
		  nextPutAll: (' is already defined in the class {1} or its hierarchy.' format: { class name })
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> violationMessageOn: aStream violators: theViolators [

	| verb |
	theViolators ifEmpty: [ ^ self ].
	verb := theViolators anyOne = violator
		        ifTrue: [ ' is already' ]
		        ifFalse: [ ' is not' ].
	^ aStream
		  nextPutAll: violator;
		  nextPutAll: verb;
		  nextPutAll: (' defined in the class {1} or its hierarchy.' format: { class name })
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> violators [
	(class hierarchyDefinesVariable: name) ifTrue: [
		violator := name.
		^ { violator } ].
	^ #()

	
]
