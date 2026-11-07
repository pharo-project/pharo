"
I am a command to rename given package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
	package:		<Package>
"
Class {
	#name : 'SycRenamePackageCommand',
	#superclass : 'SycPackageCommand',
	#instVars : [
		'package',
		'refactoringScopes'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'accessing' }
SycRenamePackageCommand >> description [

	^ 'Rename package'
]

{ #category : 'execution' }
SycRenamePackageCommand >> executeRefactoring [

	^ ((ReRenamePackageDriver newApplication: self application) scopes: refactoringScopes packageName: package name) runRefactoring
]

{ #category : 'accessing' }
SycRenamePackageCommand >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenamePackageCommand >> name [
	^'Rename'
]

{ #category : 'accessing' }
SycRenamePackageCommand >> order [

	^ 1
]

{ #category : 'execution' }
SycRenamePackageCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := self context lastSelectedPackage.
	refactoringScopes := self context refactoringScopes
]
