"
I command to launch the Abstract refactorings: create accessors and abstract the direct instance variable uses into accessors. 
"
Class {
	#name : 'SycAbstractAllInstVarAccessorsCommand',
	#superclass : 'SycClassCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycAbstractAllInstVarAccessorsCommand >> asRefactorings [

	| refactorings env |
	refactorings := classes flatCollect: [ :each |
		                each value collect: [ :var |
			                | refactoring |
			                refactoring := var
				                               createRefactoring:
				                               RBAbstractInstanceVariableRefactoring
				                               for: each key.
			                env := refactoring model.
			                refactoring ] ].
	^ refactorings
]

{ #category : 'accessing' }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuIconName [
	^ #smallAdd
]

{ #category : 'accessing' }
SycAbstractAllInstVarAccessorsCommand >> defaultMenuItemName [
	^'Abstract instance variables'
]

{ #category : 'testing' }
SycAbstractAllInstVarAccessorsCommand >> isComplexRefactoring [
	^ true
]

{ #category : 'testing' }
SycAbstractAllInstVarAccessorsCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	classes := classes collect: [:each | aToolContext currentMetaLevelOf: each].
	self selectVariables
]

{ #category : 'execution' }
SycAbstractAllInstVarAccessorsCommand >> selectVariables [
	classes := classes collect: [:aClass | | vars dialog |
		vars := aClass slots asOrderedCollection.
		dialog := StVariablesSelectionPresenter
		   label: aClass name,' - Select all variables to convert abstract'
			withItems: vars
			selecting: vars.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		aClass -> dialog selectedItems
		]
]
