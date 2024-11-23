Class {
	#name : 'ReSingleAssignmentCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'variableName',
		'definingNode'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSingleAssignmentCondition >> check [

	| searcher |
	searcher := ReAbstractTransformation new parseTreeSearcher.
	searcher
		matches: variableName , ' := ``@object'
		do: [ :aNode :answer | answer + 1 ].
	^ (searcher executeTree: definingNode initialAnswer: 0) = 1 

]

{ #category : 'accessing' }
ReSingleAssignmentCondition >> definingNode: aDefiningNode [

	definingNode := aDefiningNode
]

{ #category : 'accessing' }
ReSingleAssignmentCondition >> variableName: aVariableName [

	variableName := aVariableName 
]

{ #category : 'displaying' }
ReSingleAssignmentCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'There are multiple assignments to the variable: ';
		nextPutAll: variableName;
		cr;
		nextPutAll: 'Reduce the selection or modify the code to only assign the variable once.'
]
