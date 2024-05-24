"
I am a command to rename method protocol.

My method group should be either ClyMethodsInProtocolGroup or ClyUnclassifiedMethodGroup.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodGroup:		<ClyMethodGroup>
	newName:		<String>
"
Class {
	#name : 'ClyRenameProtocolCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'methodGroup',
		'newName'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'testing' }
ClyRenameProtocolCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isProtocolSelected
]

{ #category : 'activation' }
ClyRenameProtocolCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyFullBrowserMethodGroupContext
]

{ #category : 'activation' }
ClyRenameProtocolCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyFullBrowserMethodGroupContext
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showProtocol: newName
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> execute [

	methodGroup categorizeMethodsIn: newName
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> methodGroup [
	^ methodGroup
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> methodGroup: anObject [
	methodGroup := anObject
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> newName [
	^ newName
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> newName: anObject [
	newName := anObject
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	methodGroup := aToolContext lastSelectedMethodGroup.

	newName := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter |
		           presenter
			           protocolName: methodGroup name;
			           concernedClass: aToolContext lastSelectedClass ].
	newName = methodGroup name ifTrue: [ CmdCommandAborted signal ]
]
