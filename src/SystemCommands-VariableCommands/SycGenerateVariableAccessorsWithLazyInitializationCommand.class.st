Class {
	#name : #SycGenerateVariableAccessorsWithLazyInitializationCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'variable',
		'value'
	],
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #testing }
SycGenerateVariableAccessorsWithLazyInitializationCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : #activation }
SycGenerateVariableAccessorsWithLazyInitializationCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2 for: ClyFullBrowserVariableContext 
]

{ #category : #converting }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> asRefactorings [

	^ { (RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
		variable: variable name
		class: variable definingClass 
		classVariable: variable isClassVariable
		defaultValue: value) }
]

{ #category : #accessing }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> defaultMenuIconName [ 
	^ #smallAdd
]

{ #category : #accessing }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> defaultMenuItemName [
	^'Generate accessors (lazy)'
]

{ #category : #testing }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> isComplexRefactoring [
	^true
]

{ #category : #execution }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	variable := aToolContext lastSelectedVariable.
	value := self uiManager
		           request: 'Default value of variable accessor'
		           initialAnswer: 'nil'
		           title: 'Default variable''s value'.
	value isEmptyOrNil ifTrue: [ CmdCommandAborted signal ]
]
