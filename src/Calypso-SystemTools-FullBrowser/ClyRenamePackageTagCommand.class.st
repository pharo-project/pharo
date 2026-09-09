"
I am a command to rename class tag of the package
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classGroup:		<ClyClassGroup>
	newName:		<String>
	package:		<Package>

"
Class {
	#name : 'ClyRenamePackageTagCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'package',
		'newName',
		'packageTag'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Class groups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Class groups'
}

{ #category : 'testing' }
ClyRenamePackageTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageTagSelected
]

{ #category : 'activation' }
ClyRenamePackageTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyFullBrowserClassGroupContext
]

{ #category : 'activation' }
ClyRenamePackageTagCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyFullBrowserClassGroupContext
]

{ #category : 'execution' }
ClyRenamePackageTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showPackage: package tag: newName
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> description [ 

^'Renames class tag of the package'
]

{ #category : 'execution' }
ClyRenamePackageTagCommand >> execute [

	packageTag renameTagTo: newName
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> newName [
	^ newName
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> newName: anObject [
	newName := anObject
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> package [
	^ package
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> package: anObject [
	package := anObject
]

{ #category : 'execution' }
ClyRenamePackageTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage.
	packageTag := aToolContext lastSelectedClassGroup.
	newName := self morphicUIManager
		request: 'New name of the class tag'
		initialAnswer: packageTag name
		title: 'Rename a class tag'.
	newName isEmptyOrNil | (newName = packageTag name) ifTrue: [ CmdCommandAborted signal]
]
