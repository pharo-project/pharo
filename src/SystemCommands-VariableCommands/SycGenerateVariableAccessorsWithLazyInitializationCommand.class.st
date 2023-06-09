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

	^ (super canBeExecutedInContext: aToolContext) and: [
		  aToolContext isVariableSelected and: [
			  aToolContext isGlobalVariableSelected not ] ]
]

{ #category : #activation }
SycGenerateVariableAccessorsWithLazyInitializationCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 2 for: ClyFullBrowserVariableContext
]

{ #category : #converting }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> asRefactorings [

	^ { (self refactoringClass
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
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
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

{ #category : #'factory method' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> refactoringClass [

	^ RBCreateLazyAccessorsForVariableTransformation
]
