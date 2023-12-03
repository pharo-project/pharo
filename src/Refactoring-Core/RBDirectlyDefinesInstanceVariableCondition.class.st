Class {
	#name : 'RBDirectlyDefinesInstanceVariableCondition',
	#superclass : 'RBClassCondition',
	#instVars : [
		'instanceVariables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBDirectlyDefinesInstanceVariableCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
RBDirectlyDefinesInstanceVariableCondition >> check [
	
	violators := instanceVariables reject: [ :shared | aClass directlyDefinesInstanceVariable: shared ].
	^ violators isEmpty
]

{ #category : 'private' }
RBDirectlyDefinesInstanceVariableCondition >> errorBlock [
	^ [ aClass printString
				, ' <1?: > directly define <1?s:> instance variable ' , instanceVariables ]
]

{ #category : 'accessing' }
RBDirectlyDefinesInstanceVariableCondition >> instanceVariables: aColOfStrings [

	instanceVariables := aColOfStrings
]
