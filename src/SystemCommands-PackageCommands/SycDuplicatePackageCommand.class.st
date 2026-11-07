"
I am a command to copy given package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
	package:		<Package>
"
Class {
	#name : 'SycDuplicatePackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands-Scope',
	#package : 'SystemCommands-PackageCommands',
	#tag : 'Scope'
}

{ #category : 'accessing' }
SycDuplicatePackageCommand >> description [

	^ 'Copy a given package.'
]

{ #category : 'execution' }
SycDuplicatePackageCommand >> executeRefactoring [
	"The copy is made by the dialog in #prepareFullExecution."

	
]

{ #category : 'accessing' }
SycDuplicatePackageCommand >> iconName [
	^#smallCopy
]

{ #category : 'accessing' }
SycDuplicatePackageCommand >> name [
	^'Duplicate package'
]

{ #category : 'accessing' }
SycDuplicatePackageCommand >> order [

	^ 1.1
]

{ #category : 'execution' }
SycDuplicatePackageCommand >> prepareFullExecution [

	| dialog package newName |
	super prepareFullExecution.
	package := self context lastSelectedPackage.
	newName := self morphicUIManager request: 'New name of copied package' initialAnswer: package name title: 'Copy package'.
	newName isEmptyOrNil | (newName = package name) ifTrue: [ ^ CmCommandAborted signal ].
	dialog := SycCopyClassUsingPatternPresenter openOn: package -> newName.
	dialog cancelled ifTrue: [ ^ CmCommandAborted signal ]
]
