"
I am a command to rename class tag of the package
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classGroup:		<ClyClassGroup>
	newName:		<String>
	package:		<RPackage>

"
Class {
	#name : #ClyRenameClassTagCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'package',
		'classGroup',
		'newName'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Class groups'
}

{ #category : #testing }
ClyRenameClassTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassTagSelected

]

{ #category : #activation }
ClyRenameClassTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyClassGroupContextOfFullBrowser 
]

{ #category : #activation }
ClyRenameClassTagCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyClassGroupContextOfFullBrowser
]

{ #category : #execution }
ClyRenameClassTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showPackage: package atClassTag: newName
]

{ #category : #accessing }
ClyRenameClassTagCommand >> classGroup [
	^ classGroup
]

{ #category : #accessing }
ClyRenameClassTagCommand >> classGroup: anObject [
	classGroup := anObject
]

{ #category : #accessing }
ClyRenameClassTagCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : #execution }
ClyRenameClassTagCommand >> execute [

	classGroup renameClassTagTo: newName
]

{ #category : #accessing }
ClyRenameClassTagCommand >> newName [
	^ newName
]

{ #category : #accessing }
ClyRenameClassTagCommand >> newName: anObject [
	newName := anObject
]

{ #category : #accessing }
ClyRenameClassTagCommand >> package [
	^ package
]

{ #category : #accessing }
ClyRenameClassTagCommand >> package: anObject [
	package := anObject
]

{ #category : #execution }
ClyRenameClassTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	package := aToolContext lastSelectedPackage.
	classGroup := aToolContext lastSelectedClassGroup.
	newName := UIManager default 
		request: 'New name of the class tag' 
		initialAnswer: classGroup name 
		title: 'Rename a class tag'.
	newName isEmptyOrNil | (newName = classGroup name) ifTrue: [ CmdCommandAborted signal]
]
