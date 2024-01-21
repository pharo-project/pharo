Class {
	#name : 'ReClassCondition',
	#superclass : 'ReClassNameCondition',
	#instVars : [
		'aClass'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReClassCondition class >> class: anRBClass [ 
	^ self new
		class: anRBClass;
		yourself
]

{ #category : 'accessing' }
ReClassCondition >> analysedClass [
	^ aClass
]

{ #category : 'accessing' }
ReClassCondition >> class: aRBClass [
	aClass := aRBClass
]

{ #category : 'accessing' }
ReClassCondition >> classNamed: aString [ 
	aClass := model classNamed: aString
]
