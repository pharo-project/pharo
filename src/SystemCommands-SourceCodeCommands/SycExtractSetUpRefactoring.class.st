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
	self setUpOptionToSearchInHierarchyDuring: refactoring.
	refactoring execute
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

{ #category : #executing }
SycExtractSetUpRefactoring >> setUpOptionToSearchInHierarchyDuring: refactoring [

	refactoring setOption: #searchInAllHierarchy toUse: [ 
		method origin subclasses ifNotEmpty: [ 
			UIManager default 
			confirm: 'Do you want to search occurrences in the whole hierarchy?'
			label: 'Warning'
		] ifEmpty: [ false ]
		]. 	
]
