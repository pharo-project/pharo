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
	#superclass : 'SycPackageCommand',
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
ClyRenamePackageTagCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation renamingFor: ClyFullBrowserClassGroupContext
]

{ #category : 'execution' }
ClyRenamePackageTagCommand >> applyCommandResult [
	super applyCommandResult.
	self context showPackage: package tag: newName
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> description [ 

^'Renames class tag of the package'
]

{ #category : 'execution' }
ClyRenamePackageTagCommand >> executeRefactoring [

	packageTag renameTagTo: newName
]

{ #category : 'testing' }
ClyRenamePackageTagCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassGroupContext
]

{ #category : 'accessing' }
ClyRenamePackageTagCommand >> name [
	^'Rename'
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
ClyRenamePackageTagCommand >> order [

	^ 1
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
ClyRenamePackageTagCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	packageTag := self context lastSelectedClassGroup.
	newName := self morphicUIManager request: 'New name of the class tag' initialAnswer: packageTag name title: 'Rename a class tag'.
	newName isEmptyOrNil | (newName = packageTag name) ifTrue: [ CmCommandAborted signal ]
]
