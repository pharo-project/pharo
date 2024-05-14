Class {
	#name : 'ReClassCondition',
	#superclass : 'ReClassNameCondition',
	#instVars : [
		'class'
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
	^ class
]

{ #category : 'accessing' }
ReClassCondition >> class: aRBClass [
	class := aRBClass
]

{ #category : 'accessing' }
ReClassCondition >> classNamed: aString [ 
	class := model classNamed: aString
]
