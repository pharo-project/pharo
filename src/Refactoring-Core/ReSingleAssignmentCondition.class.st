Class {
	#name : 'ReSingleAssignmentCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'parseTree',
		'variableName'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSingleAssignmentCondition >> check [

	| searcher matchesCount |
	searcher := parseTree.
	matchesCount := 0.
	searcher 
		matches: variableName , ' := ``@object'
		do: [ :aNode | matchesCount := matchesCount + 1 ].
	^ matchesCount = 1
]

{ #category : 'initialization' }
ReSingleAssignmentCondition >> initialize: aVariableName parseTree: aParseTree [

	variableName := aVariableName.
	parseTree := aParseTree
]

{ #category : 'displaying' }
ReSingleAssignmentCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'There are multiple assignments to the variable: ';
		nextPutAll: variableName;
		cr;
		nextPutAll: 'Reduce the selection or modify the code to only assign the variable once.'
]
