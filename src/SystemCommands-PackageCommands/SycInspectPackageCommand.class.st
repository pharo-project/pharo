"
Opens an inspector on the last selected package.
"
Class {
	#name : 'SycInspectPackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycInspectPackageCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: Package
]

{ #category : 'execution' }
SycInspectPackageCommand >> executeRefactoring [

	(Smalltalk tools toolNamed: #inspector) openOn: context lastSelectedPackage
]

{ #category : 'accessing' }
SycInspectPackageCommand >> icon [

	^ self iconNamed: #inspect
]

{ #category : 'accessing' }
SycInspectPackageCommand >> name [

	^ 'Inspect'
]
