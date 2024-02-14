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
ReDirectlyDefinesInstanceVariableCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReDirectlyDefinesInstanceVariableCondition >> check [
	
	violators := instanceVariables reject: [ :shared | aClass directlyDefinesInstanceVariable: shared ].
	^ violators isEmpty
]

{ #category : 'private' }
ReDirectlyDefinesInstanceVariableCondition >> errorBlock [
	^ [ aClass printString
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
					format: { violator. className });
			nextPut: Character cr ]
]
