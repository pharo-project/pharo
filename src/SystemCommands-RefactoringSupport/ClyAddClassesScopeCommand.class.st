Class {
	#name : #ClyAddClassesScopeCommand,
	#superclass : #SycClassCommand,
	#instVars : [
		'classGroup',
		'browser',
		'key'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #activation }
ClyAddClassesScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^ SycClassMenuActivation byRootGroupItemOrder: 1000 for: ClyClass asCalypsoItemContext
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
	
	scope := RBClassEnvironment onEnvironment: browser navigationEnvironment classes: classes.
	RefactoringSettings addScope: scope withKey: key.
]

{ #category : #execution }
ClyAddClassesScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	browser := aToolContext browser.
	key := UIManager default request: 'Keyword of scoper'.
	(key isEmptyOrNil or: [ RefactoringSettings invalidNames includes: key ]) ifTrue: [ CmdCommandAborted signal ].
]
