"
I am a command to remove method group with containing methods
"
Class {
	#name : #ClyRemoveMethodGroupCommand,
	#superclass : #ClyMethodGroupCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #activation }
ClyRemoveMethodGroupCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 100000 for: ClyMethodGroupContextOfFullBrowser 
]

{ #category : #activation }
ClyRemoveMethodGroupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyMethodGroupContextOfFullBrowser
]

{ #category : #accessing }
ClyRemoveMethodGroupCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
ClyRemoveMethodGroupCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
ClyRemoveMethodGroupCommand >> execute [
	
	methodGroups do: #removeWithMethods
]

{ #category : #execution }
ClyRemoveMethodGroupCommand >> prepareFullExecutionInContext: aToolContext [
	| tagsString confirmed |
	super prepareFullExecutionInContext: aToolContext.
	
	tagsString := ', ' join: (methodGroups collect: #name).
	(methodGroups anySatisfy: [ :each | each methodsSize > 0])
		ifTrue: [
			confirmed := UIManager default 
					confirm: 'Are you sure you want to remove methods tagged with ', tagsString, '?'.
			confirmed ifFalse: [CmdCommandAborted signal]]
]
