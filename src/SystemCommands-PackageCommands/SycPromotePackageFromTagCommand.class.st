"
Promotes the selected package tag to a package of its own, and shows it in the Full Browser.
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

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> applyCommandResult [

	| newPackage |
	newPackage := (package name , '-' , classTag) asPackage.
	context showPackage: newPackage
]

{ #category : 'testing' }
SycPromotePackageFromTagCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: ClyTaggedClassGroup
]

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> executeRefactoring [

	(package tagNamed: classTag) promoteAsPackage
]

{ #category : 'accessing' }
SycPromotePackageFromTagCommand >> name [

	^ 'Promote as package'
]

{ #category : 'execution' }
SycPromotePackageFromTagCommand >> prepareFullExecution [

	super prepareFullExecution.
	package := context lastSelectedPackage.
	classTag := context lastSelectedPackageTag
]
