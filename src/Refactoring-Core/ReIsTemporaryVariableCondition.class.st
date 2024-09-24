Class {
	#name : 'ReIsTemporaryVariableCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'parseTree',
		'variableName'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsTemporaryVariableCondition >> check [
	
	violators := parseTree allDefinedVariables select: [ :each | each = variableName ].
	^ violators isEmpty
]

{ #category : 'private' }
ReIsTemporaryVariableCondition >> errorBlock [

	^ [ 'The variable {1} is already defined as a temporary variable' format: { variableName } ]
]

{ #category : 'accessing' }
ReIsTemporaryVariableCondition >> parseTree: aParseTree [

	parseTree := aParseTree 
]

{ #category : 'accessing' }
ReIsTemporaryVariableCondition >> variableName: aVariableName [

	variableName := aVariableName
]

{ #category : 'displaying' }
ReIsTemporaryVariableCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		  aStream
			nextPutAll: 
				('The variable {1} is already defined as a temporary variable in the parse tree' 
					format: { violator });
			nextPut: Character cr ]
]
