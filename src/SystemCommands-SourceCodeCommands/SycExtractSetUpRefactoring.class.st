"
I am a command to extract setUp method and then replaces its occurrences
"
Class {
	#name : #SycExtractSetUpRefactoring,
	#superclass : #SycSourceCodeCmCommand,
	#instVars : [
		'method',
		'sourceNode',
		'selectedTextInterval'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

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

{ #category : #accessing }
SycExtractSetUpRefactoring >> icon [ 
	^ self iconNamed: #glamorousRestart
]

{ #category : #initialization }
SycExtractSetUpRefactoring >> initialize [
	super initialize.
	self
		description: 'Extract setUp and find occurrences.'
]

{ #category : #testing }
SycExtractSetUpRefactoring >> isApplicable [

	^ context lastSelectedMethod selector isTestSelector
]

{ #category : #accessing }
SycExtractSetUpRefactoring >> name [ 	
	^ 'Extract setUp'
]

{ #category : #preparation }
SycExtractSetUpRefactoring >> prepareFullExecution [
	super prepareFullExecution.
	method := context lastSelectedMethod.
	sourceNode := context lastSelectedSourceNode.
	selectedTextInterval := context selectedTextInterval.
]
