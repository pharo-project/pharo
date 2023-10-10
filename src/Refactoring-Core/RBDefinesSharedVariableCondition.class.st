Class {
	#name : 'RBDefinesSharedVariableCondition',
	#superclass : 'RBClassCondition',
	#instVars : [
		'sharedVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBDefinesSharedVariableCondition class >> class: aRBClass sharedVariables: aCollection [ 
	^ self new class: aRBClass; sharedVariables: aCollection ; yourself
]

{ #category : 'instance creation' }
RBDefinesSharedVariableCondition class >> classNamed: aString inModel: aRBNamespace sharedVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		sharedVariables: aCollection;
		yourself
]

{ #category : 'accessing' }
RBDefinesSharedVariableCondition >> check [
	
	violators := sharedVariables reject: [ :shared | aClass definesClassVariable: shared ].
	^ violators isEmpty
]

{ #category : 'accessing' }
RBDefinesSharedVariableCondition >> sharedVariables: aCollection [ 
	sharedVariables := aCollection
]
