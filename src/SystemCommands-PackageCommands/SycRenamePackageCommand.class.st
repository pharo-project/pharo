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
		packageName: package name) runRefactoring
]

{ #category : 'execution' }
SycRenamePackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	package := aToolContext lastSelectedPackage.
	refactoringScopes := aToolContext refactoringScopes.
	
]
