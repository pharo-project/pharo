"
I check if `name` is not already used by a variable in the `class` hierarchy
"
Class {
	#name : 'ReIsVariableNotDefinedInHierarchy',
	#superclass : 'ReVariableNameCondition',
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

	(aClass hierarchyDefinesVariable: variable) ifTrue: [
		violator := variable.
		^ false ].
	^ true
]

{ #category : 'accessing' }
ReIsVariableNotDefinedInHierarchy >> class: someClass [
	aClass := someClass
]

{ #category : 'displaying' }
ReIsVariableNotDefinedInHierarchy >> violationMessageOn: aStream [
	
	^ aStream
		  nextPutAll: violator;
		  nextPutAll: (' is already defined in the class {1} or its hierarchy.' format: { aClass name })
]
