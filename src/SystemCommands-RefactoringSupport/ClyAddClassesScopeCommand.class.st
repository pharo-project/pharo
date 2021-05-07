Class {
	#name : #ClyAddClassesScopeCommand,
	#superclass : #SycClassCommand,
	#instVars : [
		'classGroup',
		'browser'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #activation }
ClyAddClassesScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext  
]

{ #category : #accessing }
ClyAddClassesScopeCommand >> defaultMenuIconName [ 
	^ #add
]

{ #category : #accessing }
ClyAddClassesScopeCommand >> defaultMenuItemName [ 
	^'Add to scope list'
]

{ #category : #execution }
ClyAddClassesScopeCommand >> execute [
	| scope |
	
	scope := RBClassEnvironment classes: classes.
	RefactoringSettings addScope: scope
]

{ #category : #execution }
ClyAddClassesScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	browser := aToolContext browser.
]
