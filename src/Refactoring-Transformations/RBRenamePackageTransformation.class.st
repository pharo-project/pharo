"
I'm a refactoring to rename a package.

My preconditions verify that the new name is different from the current package name and is a valid name.

I change all the references of the classes that are defined within the package, and if there is a manifest, it is updated with the new name of the package. 

Usage:
```
| transformation |
transformation := (RBRenamePackageTransformation
				rename: #'Refactoring-Transformations-Tests'
				to: #'Refactoring2-Transformations-Tests1')
				generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
"
Class {
	#name : 'RBRenamePackageTransformation',
	#superclass : 'RBPackageTransformation',
	#instVars : [
		'package'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'instance creation' }
RBRenamePackageTransformation class >> model: aRBSmalltalk rename: aSymbol to: aNewName [
	^ self new
		model: aRBSmalltalk;
		packageName: aSymbol newName: aNewName;
		yourself
]

{ #category : 'instance creation' }
RBRenamePackageTransformation class >> rename: aString to: aNewName [
	^ self new
		packageName: aString newName: aNewName
]

{ #category : 'preconditions' }
RBRenamePackageTransformation >> applicabilityPreconditions [

	^ super applicabilityPreconditions , {
		(RBCondition withBlock: [ newName ~= packageName ] errorString: 'The new package name is the same as the old package name.').
		(RBCondition
			   withBlock: [ "Cyril: I am not sure we should use #packageOrganizer. Maybe we should ask the environment the package manager. But currently the image does not know yet how to work with multiple package managers/modules."
				   (self packageOrganizer hasPackage: newName) not ]
			   errorString: 'The system already includes a package named ' , newName) }
]

{ #category : 'executing' }
RBRenamePackageTransformation >> buildTransformations [

	transformations := OrderedCollection new.

	self
		renamePackage;
		renameManifestClass.

	^ transformations
]

{ #category : 'executing' }
RBRenamePackageTransformation >> manifestClassNameFor: aPackageName [
	"Returns a symbol representing a suitable name for a Manifest class for the given package"

	^('Manifest', (aPackageName select: [:each | each isAlphaNumeric ])) asSymbol
]

{ #category : 'executing' }
RBRenamePackageTransformation >> packageName: aName newName: aNewName [
	packageName := aName asSymbol.
	package := self model packageNamed: packageName.
	newName := aNewName asSymbol
]

{ #category : 'executing' }
RBRenamePackageTransformation >> renameManifestClass [
	| manifest |
	manifest := package realPackage packageManifestOrNil.
	manifest ifNotNil: [
	transformations add: ( ReRenameClassRefactoring
		model: self model
		rename: manifest name
		to: (self manifestClassNameFor: newName)) ]
]

{ #category : 'executing' }
RBRenamePackageTransformation >> renamePackage [
	self model renamePackage: package to: newName
]

{ #category : 'storing' }
RBRenamePackageTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' rename: '.
	aStream nextPutAll: package name.
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newName;
		nextPut: $)
]
