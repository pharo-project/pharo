"
I'm a condition that checks if given variable name has local conflicts with other variables/nodes in given parseTree.
"
Class {
	#name : 'ReLocalNameConflictCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'variableName',
		'definingNode',
		'parseTree'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReLocalNameConflictCondition class >> class: aClass variableName: aNewName definingNode: aNode parseTree: aParseTree [
    ^ self new
        class: aClass;
        variableName: aNewName definingNode: aNode parseTree: aParseTree;
        yourself
]

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

    (definingNode exactNodeDefines: variableName) ifTrue: [
        violators addIfNotPresent: variableName ].

    (parseTree withAllChildren anySatisfy: [ :node |
        (node exactNodeDefines: variableName) and: [
            (node withAllChildren includes: definingNode) 
            or: [ definingNode withAllChildren includes: node ] ] ]) 
        ifTrue: [ violators addIfNotPresent: variableName ].

    ^ violators isEmpty
]

{ #category : 'instance creation' }
ReLocalNameConflictCondition >> variableName: aNewName definingNode: aNode parseTree: aParseTree [

	variableName := aNewName.
	definingNode := aNode.
	parseTree := aParseTree
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
