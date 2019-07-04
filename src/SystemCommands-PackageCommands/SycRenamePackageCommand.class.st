"
I am a command to rename given package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
	package:		<RPackage>
"
Class {
	#name : #SycRenamePackageCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'package',
		'newName'
	],
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #testing }
SycRenamePackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : #accessing }
SycRenamePackageCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : #execution }
SycRenamePackageCommand >> execute [
	
	package renameTo: newName
]

{ #category : #accessing }
SycRenamePackageCommand >> newName [
	^ newName
]

{ #category : #accessing }
SycRenamePackageCommand >> newName: anObject [
	newName := anObject
]

{ #category : #accessing }
SycRenamePackageCommand >> package [
	^ package
]

{ #category : #accessing }
SycRenamePackageCommand >> package: anObject [
	package := anObject
]

{ #category : #execution }
SycRenamePackageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	package := aToolContext lastSelectedPackage.	
	newName := UIManager default 
		request: 'New name of the package' 
		initialAnswer: package name 
		title: 'Rename a package'.
	newName isEmptyOrNil | (newName = package name) ifTrue: [ ^ CmdCommandAborted signal ]
]
