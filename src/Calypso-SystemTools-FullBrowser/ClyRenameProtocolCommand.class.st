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
		'newName',
		'selectedClass'
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

{ #category : 'accessing' }
ClyRenameProtocolCommand >> description [ 

^'Renames method protocol.'
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> execute [
	"The basic implementation iterates over the methods selected and update their protocol. Then Calypso is updated via the announcements of recategorization.
	But this leads to a problem when there is no method in the selected protocol. In that case I'm updating directly the class to remove the old protocol and add the new one, which will update Calypso.
	I hesitated to rename the protocol in the latest selected class but we could have multiple classes selected so I'm keeping part of the old implementation instead."

	methodGroup isEmpty
		ifTrue: [
			selectedClass
				removeProtocol: methodGroup name;
				addProtocol: newName ]
		ifFalse: [ methodGroup categorizeMethodsIn: newName ]
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

	selectedClass := aToolContext lastSelectedClass.
	methodGroup := aToolContext lastSelectedMethodGroup.

	newName := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter |
		           presenter
			           protocolName: methodGroup name;
			           concernedClass: aToolContext lastSelectedClass ].
	newName = methodGroup name ifTrue: [ CmdCommandAborted signal ]
]
