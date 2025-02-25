"
I am a command to rename given package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
	package:		<Package>
"
Class {
	#name : 'SycRenamePackageCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'package',
		'newName'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycRenamePackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : 'accessing' }
SycRenamePackageCommand >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenamePackageCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'accessing' }
SycRenamePackageCommand >> description [

	^ 'Rename package'
]

{ #category : 'execution' }
SycRenamePackageCommand >> execute [

	(RBRenamePackageRefactoring rename: package name to: newName) execute
]

{ #category : 'execution' }
SycRenamePackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage.
	newName := self morphicUIManager
		request: 'New name of the package'
		initialAnswer: package name
		title: 'Rename a package'.
	newName isEmptyOrNil | (newName = package name) ifTrue: [ ^ CmdCommandAborted signal ]
]
