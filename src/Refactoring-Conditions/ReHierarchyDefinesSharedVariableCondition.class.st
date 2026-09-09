Class {
	#name : 'ReHierarchyDefinesSharedVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'sharedVariables'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReHierarchyDefinesSharedVariableCondition >> check [
	
	violators := sharedVariables reject: [ :var | class hierarchyDefinesClassVariable: var ].
	^ violators isEmpty
]

{ #category : 'accessing' }
ReHierarchyDefinesSharedVariableCondition >> sharedVariables: aCollection [ 
	sharedVariables := aCollection
]

{ #category : 'displaying' }
ReHierarchyDefinesSharedVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The shared variable {1} is not defined in the class {2} or its subclasses' 
					format: { violator. className });
			nextPut: Character cr ]
]
