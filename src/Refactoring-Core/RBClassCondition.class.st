Class {
	#name : 'RBClassCondition',
	#superclass : 'RBClassNameCondition',
	#instVars : [
		'aClass'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBClassCondition class >> class: anRBClass [ 
	^ self new
		class: anRBClass;
		yourself
]

{ #category : 'accessing' }
RBClassCondition >> class: aRBClass [
	aClass := aRBClass
]

{ #category : 'accessing' }
RBClassCondition >> classNamed: aString [ 
	aClass := model classNamed: aString
]
