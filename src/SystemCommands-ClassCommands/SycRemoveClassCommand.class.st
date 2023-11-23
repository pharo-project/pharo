Class {
	#name : 'SycRemoveClassCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycRemoveClassCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveClassCommand >> defaultMenuItemName [
	^'Remove Class'
]

{ #category : 'execution' }
SycRemoveClassCommand >> execute [

	(RBRemoveClassDriver new
				 scopes: toolContext refactoringScopes
				 classes:  classes) runRefactoring
]

{ #category : 'testing' }
SycRemoveClassCommand >> isComplexRefactoring [
	^ false
]

{ #category : 'execution' }
SycRemoveClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]
