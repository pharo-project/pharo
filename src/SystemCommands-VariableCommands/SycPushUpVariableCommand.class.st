"
I am a command to push up given variables
"
Class {
	#name : 'SycPushUpVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#instVars : [
		'refactoringScopes',
		'variable'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycPushUpVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.3 for: ClySourceCodeContext
]

{ #category : 'execution' }
SycPushUpVariableCommand >> defaultMenuIconName [
	^ #up
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> defaultMenuItemName [
	^'(R) Push up'
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> executeRefactorings [

	(RePushUpVariableDriver new
		scopes: refactoringScopes
		variable: variable actualVariable name
		to: variable definingClass superclass) runRefactoring
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> isComplexRefactoring [

	^ false
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes.
	toolContext := aToolContext.
	variable := toolContext lastSelectedVariable.
	
]
