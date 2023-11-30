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
	
	(class definesVariable: name) ifTrue: [ violators := { class } . ^ false ].
	
	(class hierarchyDefinesVariable: name) ifTrue: [
		violators := (class allSubclasses select: [ :each | each directlyDefinesVariable: name ]).
		^ false ].
	
	^ true
]

{ #category : 'accessing' }
RBIsVariableNotDefinedInHierarchy >> class: aClass [
	class := aClass
]

{ #category : 'accessing' }
RBIsVariableNotDefinedInHierarchy >> errorString [

	^ 'The class {1}, in the hierarchy, already defines a variable named {2}'
		  format: { violators first. name }
]
