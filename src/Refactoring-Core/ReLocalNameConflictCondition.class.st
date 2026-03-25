"
I'm a condition that checks if given variable name has local conflicts with other variables/nodes in given parseTree.
"
Class {
	#name : 'ReLocalNameConflictCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'variableName',
		'parseTree'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReLocalNameConflictCondition class >> class: aRBAbstractClass variableName: aVariableName parseTree: aParseTree [ 
	^ self new class: aRBAbstractClass ; variableName: aVariableName parseTree: aParseTree; yourself
]

{ #category : 'instance creation' }
ReLocalNameConflictCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReLocalNameConflictCondition >> check [

    (parseTree whichUpNodeDefines: variableName) ifNotNil: [
        violators addIfNotPresent: variableName ].

    (parseTree allDefinedVariables includes: variableName) ifTrue: [
        violators addIfNotPresent: variableName ].

    ^ violators isEmpty

]

{ #category : 'instance creation' }
ReLocalNameConflictCondition >> variableName: aVariableName parseTree: aParseTree [

	variableName := aVariableName.
	parseTree := aParseTree.
]

{ #category : 'displaying' }
ReLocalNameConflictCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		aStream
			nextPutAll: ('The variable {1} is invalid in the context of the class {2}'
         		format: { violator. class name });
			nextPut: Character cr ].
]
