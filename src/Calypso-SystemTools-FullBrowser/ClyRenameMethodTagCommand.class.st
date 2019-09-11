"
I am a command to rename method tag (protocol).

My method group should be either ClyTaggedMethodGroup or ClyNoTagMethodGroup.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodGroup:		<ClyMethodGroup>
	newName:		<String>
"
Class {
	#name : #ClyRenameMethodTagCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'methodGroup',
		'newName'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #testing }
ClyRenameMethodTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isMethodTagSelected
]

{ #category : #activation }
ClyRenameMethodTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyMethodGroupContextOfFullBrowser 
]

{ #category : #activation }
ClyRenameMethodTagCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyMethodGroupContextOfFullBrowser
]

{ #category : #execution }
ClyRenameMethodTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showMethodTag: newName
]

{ #category : #accessing }
ClyRenameMethodTagCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : #execution }
ClyRenameMethodTagCommand >> execute [
	
	methodGroup renameMethodTagTo: newName asSymbol
]

{ #category : #accessing }
ClyRenameMethodTagCommand >> methodGroup [
	^ methodGroup
]

{ #category : #accessing }
ClyRenameMethodTagCommand >> methodGroup: anObject [
	methodGroup := anObject
]

{ #category : #accessing }
ClyRenameMethodTagCommand >> newName [
	^ newName
]

{ #category : #accessing }
ClyRenameMethodTagCommand >> newName: anObject [
	newName := anObject
]

{ #category : #execution }
ClyRenameMethodTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	methodGroup := aToolContext lastSelectedMethodGroup.
	newName := aToolContext 
		requestSingleMethodTag: 'New name of protocol' suggesting: methodGroup name.
	newName = methodGroup name ifTrue: [ CmdCommandAborted signal ]
]
