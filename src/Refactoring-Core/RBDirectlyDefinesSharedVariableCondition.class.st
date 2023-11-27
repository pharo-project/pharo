Class {
	#name : 'RBDirectlyDefinesSharedVariableCondition',
	#superclass : 'RBClassCondition',
	#instVars : [
		'sharedVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBDirectlyDefinesSharedVariableCondition class >> class: aRBClass sharedVariables: aCollection [ 
	^ self new class: aRBClass; sharedVariables: aCollection ; yourself
]

{ #category : 'instance creation' }
RBDirectlyDefinesSharedVariableCondition class >> classNamed: aString inModel: aRBNamespace sharedVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		sharedVariables: aCollection;
		yourself
]

{ #category : 'accessing' }
RBDirectlyDefinesSharedVariableCondition >> check [
	
	violators := sharedVariables reject: [ :shared | aClass directlyDefinesClassVariable: shared ].
	^ violators isEmpty
]

{ #category : 'accessing' }
RBDirectlyDefinesSharedVariableCondition >> sharedVariables: aCollection [ 
	sharedVariables := aCollection
]
