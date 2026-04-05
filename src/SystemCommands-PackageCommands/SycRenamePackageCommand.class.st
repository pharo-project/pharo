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
		'newName',
		'refactoringScopes'
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

	^ ((ReRenamePackageDriver newApplication: self application)
		scopes: refactoringScopes
		packageName: package name
		newName: newName) runRefactoring
]

{ #category : 'execution' }
SycRenamePackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage.
	refactoringScopes := aToolContext refactoringScopes.
	newName := self morphicUIManager
		request: 'New name of the package'
		initialAnswer: package name
		title: 'Rename a package'.
	newName isEmptyOrNil ifTrue: [ ^ CmdCommandAborted signal ]
]
