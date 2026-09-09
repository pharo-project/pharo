"
This condition checks whether a class or its subclasses defines a variable.
"
Class {
	#name : 'ReHierarchyDefinesInstanceVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReHierarchyDefinesInstanceVariableCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReHierarchyDefinesInstanceVariableCondition >> check [
	
	violators := instanceVariables reject: [ :var | class hierarchyDefinesInstanceVariable: var ].
	^ violators isEmpty
]

{ #category : 'accessing' }
ReHierarchyDefinesInstanceVariableCondition >> instanceVariables: aColOfStrings [

	instanceVariables := aColOfStrings
]

{ #category : 'displaying' }
ReHierarchyDefinesInstanceVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is not defined in the class {2} or its subclasses' 
					format: { violator. className });
			nextPut: Character cr ]
]
