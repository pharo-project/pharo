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
	self initializeDefaultOptionsOf: refactoring.

	^ { refactoring }
]

{ #category : 'accessing' }
SycExtractSetUpCommand >> defaultMenuIconName [
	^ #glamorousRestart
]

{ #category : 'accessing' }
SycExtractSetUpCommand >> defaultMenuItemName [
	^ 'Extract setUp'
]

{ #category : 'preparation' }
SycExtractSetUpCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.

	selectedTextInterval := aSourceCodeContext selectedTextInterval
]
