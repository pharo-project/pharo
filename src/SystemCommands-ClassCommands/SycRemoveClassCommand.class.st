Class {
	#name : 'SycRemoveClassCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'activation' }
SycRemoveClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserClassContext
]

{ #category : 'activation' }
SycRemoveClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
SycRemoveClassCommand >> defaultMenuIconName [
	^#removeIcon
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
