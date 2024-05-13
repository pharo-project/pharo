Class {
	#name : 'ReDirectlyDefinesInstanceVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReDirectlyDefinesInstanceVariableCondition class >> class: aRBAbstractClass inModel: aRBNamespace instanceVariables: aCollection [

	^ self new
		  class: aRBAbstractClass
		  inModel: aRBNamespace
		  instanceVariables: aCollection;
		  yourself
]

{ #category : 'instance creation' }
ReDirectlyDefinesInstanceVariableCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReDirectlyDefinesInstanceVariableCondition >> check [
	
	violators := instanceVariables reject: [ :shared | class directlyDefinesInstanceVariable: shared ].
	^ violators isEmpty
]

{ #category : 'instance creation' }
ReDirectlyDefinesInstanceVariableCondition >> class: aRBAbstractClass inModel: aRBNamespace instanceVariables: aCollection [ 

	class := aRBAbstractClass.
	model := aRBNamespace.
	instanceVariables := aCollection 
	
]

{ #category : 'private' }
ReDirectlyDefinesInstanceVariableCondition >> errorBlock [
	^ [ class printString
				, ' <1?: > directly define <1?s:> instance variable ' , instanceVariables ]
]

{ #category : 'accessing' }
ReDirectlyDefinesInstanceVariableCondition >> instanceVariables: aColOfStrings [

	instanceVariables := aColOfStrings
]

{ #category : 'displaying' }
ReDirectlyDefinesInstanceVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is not directly defined in the class {2}' 
					format: { violator. class name });
			nextPut: Character cr ]
]
