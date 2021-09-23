Class {
	#name : #RBRenamePackageTransformation,
	#superclass : #RBPackageTransformation,
	#instVars : [
		'package'
	],
	#category : #'Refactoring2-Transformations-Model'
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

{ #category : #'accessing - manifest' }
RBRenamePackageTransformation >> manifestClassNameFor: aPackageName [
	"Returns a symbol representing a suitable name for a Manifest class for the given package"
	
	^('Manifest', (aPackageName select: [:each | each isAlphaNumeric ])) asSymbol
]

{ #category : #initialization }
RBRenamePackageTransformation >> packageName: aName newName: aNewName [ 
	packageName := aName asSymbol.
	package := self model packageNamed: packageName.
	newName := aNewName asSymbol
]

{ #category : #preconditions }
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
		rename: manifest
		to: (self manifestClassNameFor: newName)) ]

]

{ #category : #executing }
RBRenamePackageTransformation >> renamePackage [
	self model renamePackage: package to: newName
]

{ #category : #printing }
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
