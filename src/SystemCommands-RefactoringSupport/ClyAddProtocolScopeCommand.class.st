Class {
	#name : #ClyAddProtocolScopeCommand,
	#superclass : #ClyMethodGroupCommand,
	#instVars : [
		'class',
		'browser',
		'key'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #activation }
ClyAddProtocolScopeCommand class >> fullBrowserMethodGroupMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserMethodGroupContext 
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> defaultMenuIconName [ 
	^ #add
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> defaultMenuItemName [ 
	^'Add to scope list'
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> execute [
	| scope protocols |
	protocols := methodGroups collect: [:each | each tag].
	scope := RBProtocolEnvironment onEnvironment: browser navigationEnvironment class: class protocols: protocols.
	RefactoringSettings addScope: scope withKey: key.
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	class := aToolContext lastSelectedClass.
	browser := aToolContext browser.
	key := UIManager default request: 'Keyword of scoper'.
	(key isEmptyOrNil or: [ RefactoringSettings invalidNames includes: key ]) ifTrue: [ CmdCommandAborted signal ].
]
