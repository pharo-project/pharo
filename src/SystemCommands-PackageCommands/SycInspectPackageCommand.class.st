"
I am a command to inspect given package.

Internal Representation and Key Implementation Points.

Instance Variables

package:		<Package>
"
Class {
	#name : 'SycInspectPackageCommand',
	#superclass : 'SycPackageExtraCmCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'testing' }
SycInspectPackageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageSelected
]

{ #category : 'testing' }
SycInspectPackageCommand >> canBeExecuted [
	^ context isAboutBrowserItemType: Package
]

{ #category : 'accessing' }
SycInspectPackageCommand >> description [

	^ 'Inspect package'
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
	^'Inspect'
]
