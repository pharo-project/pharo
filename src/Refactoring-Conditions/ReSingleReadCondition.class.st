"
I'm a condition to check that given variable name is not read more than once in given defining node.
"
Class {
	#name : 'ReSingleReadCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'variableName',
		'definingNode'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReSingleReadCondition >> check [

	| searcher assignSearcher |
	searcher := ReAbstractTransformation new parseTreeSearcher.
	searcher matches: variableName do: [ :aNode :answer | answer + 1 ].

	assignSearcher := ReAbstractTransformation new parseTreeSearcher.
	assignSearcher matches: variableName , ' := ``@object' do: [ :aNode :answer | answer + 1 ].

	^ (searcher executeTree: definingNode initialAnswer: 0) - (assignSearcher executeTree: definingNode initialAnswer: 0) <= 1
]

{ #category : 'accessing' }
ReSingleReadCondition >> definingNode: aDefiningNode [

	definingNode := aDefiningNode
]

{ #category : 'accessing' }
ReSingleReadCondition >> variableName: aVariableName [

	variableName := aVariableName 
]

{ #category : 'displaying' }
ReSingleReadCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll: 'Cannot inline ''';
		nextPutAll: variableName;
		nextPutAll: ''': it is referenced more than once.';
		cr;
		nextPutAll: 'Inlining would duplicate the initializer expression and may change program behavior.'
]
