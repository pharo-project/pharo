"
This condition checks whether a class or its subclasses defines a variable.
"
Class {
	#name : 'ReHierarchyDefinesInstanceVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
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

{ #category : 'private' }
ReHierarchyDefinesInstanceVariableCondition >> errorBlock [
	^ [ class printString
				, ' <1?: > define <1?s:> instance variable ' , instanceVariables ]
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
