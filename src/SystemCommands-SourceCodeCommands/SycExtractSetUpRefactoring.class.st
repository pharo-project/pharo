"
I am a command to extract setUp method and then replaces its occurrences
"
Class {
	#name : #SycExtractSetUpRefactoring,
	#superclass : #SycSourceCodeRefactoringCommand,
	#instVars : [
		'selectedTextInterval'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #testing }
SycExtractSetUpRefactoring class >> canBeExecutedInContext: aToolContext [
	super canBeExecutedInContext: aToolContext.
	
	^aToolContext isMethodSelected not and: [aToolContext lastSelectedMethod selector isTestSelector]
]

{ #category : #executing }
SycExtractSetUpRefactoring >> asRefactorings [ 
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

{ #category : #accessing }
SycExtractSetUpRefactoring >> defaultMenuIconName [ 
	^ #glamorousRestart
]

{ #category : #accessing }
SycExtractSetUpRefactoring >> defaultMenuItemName [ 	
	^ 'Extract setUp'
]

{ #category : #preparation }
SycExtractSetUpRefactoring >> execute [
	self executeRefactoring: self asRefactorings
]

{ #category : #executing }
SycExtractSetUpRefactoring >> executeRefactoring [ 
	| selectedInterval refactoring |
	selectedInterval := selectedTextInterval ifEmpty: [ 
		                    sourceNode sourceInterval ].
	refactoring := RBExtractSetUpMethodAndOccurrences 
		extract: selectedInterval
		from: method selector
		in: method origin.
	self initializeDefaultOptionsOf: refactoring.
	refactoring execute
]

{ #category : #preparation }
SycExtractSetUpRefactoring >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	
	selectedTextInterval := aSourceCodeContext selectedTextInterval.
]
