Class {
	#name : 'SycRemoveClassCommandRB2',
	#superclass : 'SycClassCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'activation' }
SycRemoveClassCommandRB2 class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserClassContext
]

{ #category : 'activation' }
SycRemoveClassCommandRB2 class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserClassContext
]

{ #category : 'accessing' }
SycRemoveClassCommandRB2 >> defaultMenuIconName [
	^#removeIcon
]

{ #category : 'accessing' }
SycRemoveClassCommandRB2 >> defaultMenuItemName [
	^'Remove Class 2 (refactoring)'
]

{ #category : 'execution' }
SycRemoveClassCommandRB2 >> execute [

	(RBRemoveClassDriver new
				 scopes: toolContext refactoringScopes
				 classes:  classes) runRefactoring
]

{ #category : 'testing' }
SycRemoveClassCommandRB2 >> isComplexRefactoring [
	^false
]

{ #category : 'execution' }
SycRemoveClassCommandRB2 >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
]
