"
I am a command to remove method group with containing methods
"
Class {
	#name : 'ClyRemoveMethodGroupCommand',
	#superclass : 'ClyMethodGroupCommand',
	#instVars : [
		'selectedClasses'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'activation' }
ClyRemoveMethodGroupCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 100000 for: ClyFullBrowserMethodGroupContext
]

{ #category : 'activation' }
ClyRemoveMethodGroupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : 'execution' }
ClyRemoveMethodGroupCommand >> execute [

	methodGroups do: [ :group |
		group
			removeWithMethods;
			removeProtocolFrom: selectedClasses ]
]

{ #category : 'execution' }
ClyRemoveMethodGroupCommand >> prepareFullExecutionInContext: aToolContext [

	| tagsString confirmed |
	super prepareFullExecutionInContext: aToolContext.
	selectedClasses := aToolContext  selectedClasses.
	
	tagsString := ', ' join: (methodGroups collect: [ :each | each name ]).
	(methodGroups anySatisfy: [ :each | each methodsSize > 0 ]) ifFalse: [ ^ self ].
	
	confirmed := self confirm: 'Are you sure you want to remove methods from ' , tagsString , '?'.
	confirmed ifFalse: [ CmdCommandAborted signal ]
]
