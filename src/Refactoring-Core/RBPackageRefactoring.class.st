"
I am an abstract base class for package refactorings.

All that I provide is the package name, my subclass refactorings are operating on, and a instance creation method for setting the package name and an initial namespace model.
"
Class {
	#name : 'RBPackageRefactoring',
	#superclass : 'RBRefactoring',
	#instVars : [
		'packageName',
		'newName'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'testing' }
RBPackageRefactoring class >> isAbstract [

	^ self == RBPackageRefactoring
]

{ #category : 'instance creation' }
RBPackageRefactoring class >> model: aRBModel packageName: aName [
	^ self new
		model: aRBModel;
		packageName: aName;
		yourself
]

{ #category : 'instance creation' }
RBPackageRefactoring class >> packageName: aName [
	^ self new
		packageName: aName
]

{ #category : 'preconditions' }
RBPackageRefactoring >> applicabilityPreconditions [

	^ { (RBCondition
		   withBlock: [ "Cyril: I am not sure we should use #packageOrganizer. Maybe we should ask the environment the package manager. But currently the image does not know yet how to work with multiple package managers/modules."
			   self packageOrganizer hasPackage: packageName ]
		   errorString:
		   'The package ' , packageName , ' does not exists in the system') }
]

{ #category : 'accessing' }
RBPackageRefactoring >> newName [

	^ newName
]

{ #category : 'accessing' }
RBPackageRefactoring >> newName: anObject [

	newName := anObject
]

{ #category : 'accessing' }
RBPackageRefactoring >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBPackageRefactoring >> packageName: anObject [

	packageName := anObject
]
