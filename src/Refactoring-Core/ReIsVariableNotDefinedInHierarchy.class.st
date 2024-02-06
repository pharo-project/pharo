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

{ #category : 'checking' }
ReIsVariableNotDefinedInHierarchy >> check [

	(class hierarchyDefinesVariable: name) ifTrue: [
		violator := name.
		^ false ].
	^ true
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> class: aClass [
	class := aClass
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> violationMessageOn: aStream [
	
	^ aStream
		  nextPutAll: violator;
		  nextPutAll: (' is already defined in the class {1} or its hierarchy.' format: { class name })
]
