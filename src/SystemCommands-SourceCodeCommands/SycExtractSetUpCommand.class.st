"
I am a command to extract setUp method and then replaces its occurrences
"
Class {
	#name : 'SycExtractSetUpCommand',
	#superclass : 'SycTestCommand',
	#instVars : [
		'selectedTextInterval'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'executing' }
SycExtractSetUpCommand >> asRefactorings [
	| selectedInterval refactoring |
	selectedInterval := selectedTextInterval ifEmpty: [
		                    sourceNode sourceInterval ].
	refactoring := RBExtractSetUpMethodAndOccurrences
		extract: selectedInterval
		from: method selector
		in: method origin.
	
	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractSetUpCommand >> iconName [
	^ #restart
]

{ #category : 'accessing' }
SycExtractSetUpCommand >> name [
	^ 'Extract setUp'
]

{ #category : 'preparation' }
SycExtractSetUpCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.

	selectedTextInterval := aSourceCodeContext selectedTextInterval
]
