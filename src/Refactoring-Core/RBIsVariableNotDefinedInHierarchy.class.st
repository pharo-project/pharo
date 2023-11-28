"
I check if `name` is not already used by a variable in the `class` hierarchy
"
Class {
	#name : 'RBIsVariableNotDefinedInHierarchy',
	#superclass : 'RBVariableNameCondition',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBIsVariableNotDefinedInHierarchy class >> name: aString class: aClass [
	^ (self name: aString)
		class: aClass;
		yourself
]

{ #category : 'checking' }
RBIsVariableNotDefinedInHierarchy >> check [

	^ (class hierarchyDefinesVariable: name) not
]

{ #category : 'accessing' }
RBIsVariableNotDefinedInHierarchy >> class: aClass [
	class := aClass
]
