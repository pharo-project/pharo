"
I am an abstract base class for package transformations.

All that I provide is the package name, my subclass transformations are operating on, and a instance creation method for setting the package name and an initial namespace model.
"
Class {
	#name : 'RBPackageTransformation',
	#superclass : 'RBCompositeTransformation',
	#instVars : [
		'packageName',
		'newName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'accessing' }
RBPackageTransformation class >> model: aRBModel packageName: aName [
	^ self new
		model: aRBModel;
		packageName: aName;
		yourself
]

{ #category : 'accessing' }
RBPackageTransformation class >> packageName: aName [
	^ self new
		packageName: aName
]

{ #category : 'preconditions' }
RBPackageTransformation >> applicabilityPreconditions [

	^ { (RBCondition
		   withBlock: [ "Cyril: I am not sure we should use #packageOrganizer. Maybe we should ask the environment the package manager. But currently the image does not know yet how to work with multiple package managers/modules."
			   self packageOrganizer hasPackage: packageName ]
		   errorString:
		   'The package ' , packageName , ' does not exsit in the system.') }
]

{ #category : 'executing' }
RBPackageTransformation >> packageName [

	^ packageName
]

{ #category : 'executing' }
RBPackageTransformation >> packageName: anObject [

	packageName := anObject
]
