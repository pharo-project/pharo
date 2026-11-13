Class {
	#name : 'SycGenerateVariableAccessorsWithLazyInitializationCommand',
	#superclass : 'SycRefactorVariableCommand',
	#instVars : [
		'variable',
		'value'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand class >> canBeExecutedInContext: aToolContext [

	^ (super canBeExecutedInContext: aToolContext) and: [
		  aToolContext isVariableSelected and: [
			  aToolContext isGlobalVariableSelected not ] ]
]

{ #category : 'converting' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> asRefactorings [

	^ { (self refactoringClass
		variable: variable name
		class: variable definingClass
		classVariable: variable isClassVariable
		defaultValue: value) }
]

{ #category : 'accessing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> fullBrowserOrder [

	^ 2
]

{ #category : 'accessing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> iconName [
	^ #smallAdd
]

{ #category : 'testing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserVariableContext
]

{ #category : 'testing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> isComplexRefactoring [
	^true
]

{ #category : 'accessing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> name [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'accessing' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> order [

	^ 2
]

{ #category : 'preparation' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> prepareFullExecution [

	super prepareFullExecution.
	variable := self context lastSelectedVariable.
	value := self context tool morphicUIManager
		request: 'Default value of variable accessor'
		initialAnswer: 'nil'
		title: 'Default variable''s value'.
	value isEmptyOrNil ifTrue: [ CmCommandAborted signal ]
]

{ #category : 'factory method' }
SycGenerateVariableAccessorsWithLazyInitializationCommand >> refactoringClass [

	^ RBCreateLazyAccessorsForVariableTransformation
]
