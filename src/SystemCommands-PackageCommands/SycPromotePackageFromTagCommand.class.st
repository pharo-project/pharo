"
I am a command to promote given package and tag to be separate package.

Internal Representation and Key Implementation Points.

    Instance Variables
	classTag:		<Symbol>
	package:		<Package>

"
Class {
	#name : 'SycPromotePackageFromTagCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#instVars : [
		'package',
		'classTag'
	],
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycPromotePackageFromTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected and: [aToolContext isPackageTagSelected]
]

{ #category : 'executing' }
SycPromotePackageFromTagCommand >> applyCommandResult [
	| newPackage |
	newPackage := (package name, '-', classTag) asPackage.
	context showPackage: newPackage
]

{ #category : 'testing' }
SycPromotePackageFromTagCommand >> canBeExecuted [
	^ context isAboutBrowserItemType: ClyTaggedClassGroup
]

{ #category : 'executing' }
SycPromotePackageFromTagCommand >> executeRefactoring [
	(package tagNamed: classTag) promoteAsPackage
]

{ #category : 'accessing' }
SycPromotePackageFromTagCommand >> name [
	^'Promote as package'
]

{ #category : 'preparation' }
SycPromotePackageFromTagCommand >> prepareFullExecution [
	super prepareFullExecution.
	package := context lastSelectedPackage.
	classTag := context lastSelectedPackageTag
]
