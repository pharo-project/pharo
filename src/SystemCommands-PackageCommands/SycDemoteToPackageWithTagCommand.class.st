"
Demotes the selected packages whose name contains a dash to a package with a tag, and shows the resulting package in the Full Browser.
"
Class {
	#name : 'SycDemoteToPackageWithTagCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'execution' }
SycDemoteToPackageWithTagCommand >> applyCommandResult [

	| lastPackage newPackage tag |
	lastPackage := self packages
		               detect: [ :pack | pack name includes: $- ]
		               ifNone: [ ^ self ].
	newPackage := (lastPackage name copyUpToLast: $-) asPackage.
	tag := lastPackage name last: lastPackage name size - newPackage name size - 1.
	context showPackage: newPackage tag: tag asSymbol
]

{ #category : 'testing' }
SycDemoteToPackageWithTagCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycDemoteToPackageWithTagCommand >> description [

	^ 'Demotes the selected packages whose name contains a dash to a package with a tag, and shows the resulting package in the Full Browser.'
]

{ #category : 'execution' }
SycDemoteToPackageWithTagCommand >> executeRefactoring [

	self packages
		select: [ :package | package name includes: $- ]
		thenDo: [ :package | package demoteToTagInPackage ]
]

{ #category : 'accessing' }
SycDemoteToPackageWithTagCommand >> name [

	^ 'Demote to package with tag'
]
