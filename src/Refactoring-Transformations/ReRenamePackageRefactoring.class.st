"
I'm a refactoring to rename a package.

My preconditions verify that the new name is different from the current package name and is a valid name.

I change all the references of the classes that are defined within the package, and if there is a manifest, it is updated with the new name of the package. 

Example
---------
```
(ReRenamePackageRefactoring 
				rename: (self getPackageNamed: #'Refactoring-Tests-Core')
				to: #'Refactoring-Core-Test') execute.
```
"
Class {
	#name : 'ReRenamePackageRefactoring',
	#superclass : 'RBPackageRefactoring',
	#instVars : [
		'package'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
ReRenamePackageRefactoring class >> model: aRBSmalltalk rename: aSymbol to: aNewName [
	^ self new
		model: aRBSmalltalk;
		packageName: aSymbol newName: aNewName;
		yourself
]

{ #category : 'instance creation' }
ReRenamePackageRefactoring class >> rename: aString to: aNewName [
	^ self new
		packageName: aString newName: aNewName
]

{ #category : 'preconditions' }
ReRenamePackageRefactoring >> applicabilityPreconditions [

	^ super applicabilityPreconditions, {
		 (RBCondition
			 withBlock: [ newName ~= packageName ]
			 errorString:
			 'The new package name is the same as the old package name.').
		 (RBCondition
			 withBlock: [ "Cyril: I am not sure we should use #packageOrganizer. Maybe we should ask the environment the package manager. But currently the image does not know yet how to work with multiple package managers/modules."
				 (self packageOrganizer hasPackage: newName) not ]
			 errorString:
			 'The system already includes a package named ' , newName) }
]

{ #category : 'transforming' }
ReRenamePackageRefactoring >> manifestClassNameFor: aPackageName [
	"Returns a symbol representing a suitable name for a Manifest class for the given package"

	^('Manifest', (aPackageName select: [:each | each isAlphaNumeric ])) asSymbol
]

{ #category : 'accessing' }
ReRenamePackageRefactoring >> newName: aNewName [
    newName := aNewName asSymbol
]

{ #category : 'initialization' }
ReRenamePackageRefactoring >> packageName: aName newName: aNewName [
	packageName := aName asSymbol.
	newName := aNewName asSymbol
]

{ #category : 'transforming' }
ReRenamePackageRefactoring >> prepareForExecution [
    package := self model packageNamed: packageName
]

{ #category : 'transforming' }
ReRenamePackageRefactoring >> privateTransform [

	self renamePackage.
	self renameManifestClass
]

{ #category : 'class management' }
ReRenamePackageRefactoring >> rename: aName [
    packageName := aName asSymbol
]

{ #category : 'transforming' }
ReRenamePackageRefactoring >> renameManifestClass [
	|refactoring manifest|
	manifest := package realPackage packageManifestOrNil.
	manifest ifNotNil: [
	refactoring := ReRenameClassRefactoring
		model: self model
		rename: manifest name
		to: (self manifestClassNameFor: newName).
	self generateChangesFor: refactoring]
]

{ #category : 'transforming' }
ReRenamePackageRefactoring >> renamePackage [
	self model renamePackage: package to: newName
]
