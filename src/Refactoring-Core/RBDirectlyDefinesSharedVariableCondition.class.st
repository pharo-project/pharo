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

{ #category : 'displaying' }
RBDirectlyDefinesSharedVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is not directly defined in the class {2}' 
					format: { violator. className });
			nextPut: Character cr ]
]
