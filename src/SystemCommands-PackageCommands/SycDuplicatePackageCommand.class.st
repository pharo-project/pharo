"
I am a command to copy given package.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
	package:		<Package>
"
Class {
	#name : 'SycDuplicatePackageCommand',
	#superclass : 'CmdCommand',
	#category : 'SystemCommands-PackageCommands-Scope',
	#package : 'SystemCommands-PackageCommands',
	#tag : 'Scope'
}

{ #category : 'testing' }
SycDuplicatePackageCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isPackageSelected
]

{ #category : 'accessing' }
SycDuplicatePackageCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : 'accessing' }
SycDuplicatePackageCommand >> defaultMenuItemName [
	^'Duplicate package'
]

{ #category : 'execution' }
SycDuplicatePackageCommand >> prepareFullExecutionInContext: aToolContext [

	| dialog package newName |

	super prepareFullExecutionInContext: aToolContext.

	package := aToolContext lastSelectedPackage.
	newName := self morphicUIManager
		request: 'New name of copied package'
		initialAnswer: package name
		title: 'Copy package'.
	newName isEmptyOrNil | (newName = package name) ifTrue: [ ^ CmdCommandAborted signal ].

	dialog := SycCopyClassUsingPatternPresenter openOn: package -> newName.
	dialog cancelled ifTrue: [ ^ CmdCommandAborted signal ]
]
