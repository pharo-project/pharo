Class {
	#name : #RBRenamePackageRefactoring,
	#superclass : #RBPackageRefactoring,
	#instVars : [
		'package',
		'newName'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRenamePackageRefactoring class >> model: aRBSmalltalk rename: aPkg to: aNewName [ 
	^(self new)
		model: aRBSmalltalk;
		packageName: aPkg name newName: aNewName;
		yourself
]

{ #category : #'instance creation' }
RBRenamePackageRefactoring class >> rename: aClass to: aNewName [
	^self new packageName: aClass name newName: aNewName
]

{ #category : #initialization }
RBRenamePackageRefactoring >> packageName: aName newName: aNewName [ 
	packageName := aName asSymbol.
	package := self model packageNamed: packageName.
	newName := aNewName asSymbol
]

{ #category : #preconditions }
RBRenamePackageRefactoring >> preconditions [ 
	^ self emptyCondition
]

{ #category : #transforming }
RBRenamePackageRefactoring >> renameManifestClass [ 
	|refactoring manifest|
	manifest := package packageManifestOrNil.
	refactoring := RBRenameClassRefactoring 
		rename: manifest
		to: (TheManifestBuilder manifestClassNameFor: newName).
	self performCompositeRefactoring: refactoring
]

{ #category : #transforming }
RBRenamePackageRefactoring >> renamePackage [

	self model renamePackage: package to: newName
]

{ #category : #transforming }
RBRenamePackageRefactoring >> transform [

	self renamePackage.
	self renameManifestClass.
]
