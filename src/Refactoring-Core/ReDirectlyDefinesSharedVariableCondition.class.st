Class {
	#name : 'ReDirectlyDefinesSharedVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'sharedVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReDirectlyDefinesSharedVariableCondition class >> class: aRBClass sharedVariables: aCollection [ 
	^ self new class: aRBClass; sharedVariables: aCollection ; yourself
]

{ #category : 'instance creation' }
ReDirectlyDefinesSharedVariableCondition class >> classNamed: aString inModel: aRBNamespace sharedVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		sharedVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReDirectlyDefinesSharedVariableCondition >> check [
	
	violators := sharedVariables reject: [ :shared | class directlyDefinesClassVariable: shared ].
	^ violators isEmpty
]

{ #category : 'accessing' }
ReDirectlyDefinesSharedVariableCondition >> sharedVariables: aCollection [ 
	sharedVariables := aCollection
]

{ #category : 'displaying' }
ReDirectlyDefinesSharedVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is not directly defined in the class {2}' 
					format: { violator. className });
			nextPut: Character cr ]
]
