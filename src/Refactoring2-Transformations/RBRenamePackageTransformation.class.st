"
I'm a refactoring to rename a package.

My preconditions verify that the new name is different from the current package name and is a valid name.

I change all the references of the classes that are defined within the package, and if there is a manifest, it is updated with the new name of the package. 

Usage:
| transformation |
transformation := (RBRenamePackageTransformation
				rename: (self getPackageNamed: #'Refactoring2-Transformations-Tests')
				to: #'Refactoring2-Transformations-Tests1')
				transform.
"
Class {
	#name : #RBRenamePackageTransformation,
	#superclass : #RBPackageTransformation,
	#instVars : [
		#package
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #'instance creation' }
RBRenamePackageTransformation class >> model: aRBSmalltalk rename: aSymbol to: aNewName [
	^ self new
		model: aRBSmalltalk;
		packageName: aSymbol newName: aNewName;
		yourself
]

{ #category : #'instance creation' }
RBRenamePackageTransformation class >> rename: aString to: aNewName [
	^ self new
		packageName: aString newName: aNewName
]

{ #category : #executing }
RBRenamePackageTransformation >> buildTransformations [

	transformations := OrderedCollection new.

	self
		renamePackage;
		renameManifestClass.

	^ transformations
]

{ #category : #executing }
RBRenamePackageTransformation >> manifestClassNameFor: aPackageName [
	"Returns a symbol representing a suitable name for a Manifest class for the given package"

	^('Manifest', (aPackageName select: [:each | each isAlphaNumeric ])) asSymbol
]

{ #category : #executing }
RBRenamePackageTransformation >> packageName: aName newName: aNewName [
	packageName := aName asSymbol.
	package := self model packageNamed: packageName.
	newName := aNewName asSymbol
]

{ #category : #executing }
RBRenamePackageTransformation >> preconditions [
	^ super preconditions & (RBCondition withBlock: [ newName = package name ifTrue:
		[ self refactoringError: 'Use a different name' ].
		true ]) &
	(RBCondition withBlock: [ [RPackage organizer validatePackageDoesNotExist: newName. true]
			on: Error
			do: [ :e | self refactoringError: e messageText ]
		])
]

{ #category : #executing }
RBRenamePackageTransformation >> renameManifestClass [
	| manifest |
	manifest := package realPackage packageManifestOrNil.
	manifest ifNotNil: [
	transformations add: ( RBRenameClassTransformation
		model: self model
		rename: manifest name
		to: (self manifestClassNameFor: newName)) ]
]

{ #category : #executing }
RBRenamePackageTransformation >> renamePackage [
	self model renamePackage: package to: newName
]

{ #category : #executing }
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
