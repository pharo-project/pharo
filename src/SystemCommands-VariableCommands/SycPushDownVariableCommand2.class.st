Class {
	#name : 'SycPushDownVariableCommand2',
	#superclass : 'SycRefactorVariableCommand',
	#instVars : [
		'refactoringScopes',
		'variable'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycPushDownVariableCommand2 class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.5 for: ClySourceCodeContext
]

{ #category : 'accessing' }
SycPushDownVariableCommand2 >> defaultMenuIconName [
	^ #down
]

{ #category : 'accessing' }
SycPushDownVariableCommand2 >> defaultMenuItemName [
	^'(R) Push down'
]

{ #category : 'execution' }
SycPushDownVariableCommand2 >> executeRefactorings [

	"Move this validation to the driver"
	toolContext selectedVariables size > 1 ifTrue: [
			SpAlertDialog new
				label: 'Cannot push down many variables at the same time';
				openModal.
			^ CmdCommandAborted signal ].

	(RePushDownVariableDriver new
		 scopes: refactoringScopes
		 variable: variable actualVariable name
		 shared: variable isClassVariable
		 from: variable definingClass) runRefactoring
]

{ #category : 'testing' }
SycPushDownVariableCommand2 >> isComplexRefactoring [
 
	^ false
]

{ #category : 'execution' }
SycPushDownVariableCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	refactoringScopes := aToolContext refactoringScopes.
	toolContext := aToolContext.
	variable := toolContext lastSelectedVariable
]
