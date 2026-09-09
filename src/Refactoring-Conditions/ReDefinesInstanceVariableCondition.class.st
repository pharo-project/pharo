"
Checks that a class or its superclasses define a variable.
"
Class {
	#name : 'ReDefinesInstanceVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReDefinesInstanceVariableCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReDefinesInstanceVariableCondition >> check [
	
	violators := instanceVariables reject: [ :shared | class definesInstanceVariable: shared ].
	^ violators isEmpty
]

{ #category : 'accessing' }
ReDefinesInstanceVariableCondition >> instanceVariables: aColOfStrings [

	instanceVariables := aColOfStrings
]

{ #category : 'displaying' }
ReDefinesInstanceVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is not defined in the class {2} or its subclasses' 
					format: { violator. className });
			nextPut: Character cr ]
]
