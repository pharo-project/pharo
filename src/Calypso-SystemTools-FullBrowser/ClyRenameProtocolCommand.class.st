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
	#superclass : 'ClyMethodGroupCommand',
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
ClyRenameProtocolCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation renamingFor: ClyFullBrowserMethodGroupContext
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> applyCommandResult [
	super applyCommandResult.
	self context showProtocol: newName
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> description [ 

^'Renames method protocol.'
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> executeRefactoring [

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
ClyRenameProtocolCommand >> name [
	^'Rename'
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> newName [
	^ newName
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> newName: anObject [
	newName := anObject
]

{ #category : 'accessing' }
ClyRenameProtocolCommand >> order [

	^ 1
]

{ #category : 'execution' }
ClyRenameProtocolCommand >> prepareFullExecution [

	super prepareFullExecution.
	selectedClass := self context lastSelectedClass.
	methodGroup := self context lastSelectedMethodGroup.
	newName := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter |
			           presenter
				           protocolName: methodGroup name;
				           concernedClass: self context lastSelectedClass ].
	newName = methodGroup name ifTrue: [ CmCommandAborted signal ]
]
