"
I am a command to extract selected ast node into separate method 
"
Class {
	#name : #SycExtractMethodCommand,
	#superclass : #SycSourceCodeCommand,
	#instVars : [
		'selectedTextInterval'
	],
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #execution }
SycExtractMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [
	super canBeExecutedInContext: aSourceCodeContext.
	
	^aSourceCodeContext isMethodSelected not 
]

{ #category : #accessing }
SycExtractMethodCommand >> defaultMenuIconName [ 
	^ #glamorousRestart
]

{ #category : #accessing }
SycExtractMethodCommand >> defaultMenuItemName [
	^ 'Extract method'
]

{ #category : #execution }
SycExtractMethodCommand >> execute [

	| selectedInterval dialog refactoring|
	selectedInterval := selectedTextInterval ifEmpty: [ sourceNode sourceInterval ].
	refactoring := RBExtractMethodRefactoring	
		extract: selectedInterval from: method selector in: method origin.
	self setUpOptionToUseExistingMethodDuring: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	refactoring setOption: #methodName toUse: [ :ref :methodName |
		dialog := SycMethodNameEditor openOn: methodName withRefactoring: ref.
		dialog cancelled ifTrue: [  CmdCommandAborted signal ].
		methodName].
	
	self executeRefactoring: refactoring.
]

{ #category : #private }
SycExtractMethodCommand >> executeRefactoring: refactoring [

	NautilusRefactoring new refactoringOptions: refactoring.
	[ refactoring execute.
	self findOcurrences: refactoring] 
	on: RBRefactoringError 
	do: [ :e | UIManager default alert: e messageText ].
	
]

{ #category : #execution }
SycExtractMethodCommand >> findOcurrences: aRefactoring [ 	
	|refactoring ocurrences|
	
	refactoring := RBFindAndReplaceMethodRefactoring 
		find: aRefactoring compiledMethod
		andReplaceIn: { aRefactoring method }.
	ocurrences := (refactoring findOcurrencesIn: method selector of: method origin).
	ocurrences ifNotEmpty: [ 
		(self confirm: ('We find <1s> ocurrence(s) of extracted method.
Would you like review and replace them?' expandMacrosWith: ocurrences size asString))
		 	ifTrue: [ refactoring transform ] ]
]

{ #category : #execution }
SycExtractMethodCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.
	
	selectedTextInterval := aSourceCodeContext selectedTextInterval
]

{ #category : #execution }
SycExtractMethodCommand >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :class :selector | 
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', class name]. 
]

{ #category : #execution }
SycExtractMethodCommand >> setUpOptionToUseExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #useExistingMethod toUse:  [ :ref :selector | 
		UIManager default 
			confirm: 'Do you want use existing method ', selector printString, '?'
			label: 'Warninig']. 	
	
]
