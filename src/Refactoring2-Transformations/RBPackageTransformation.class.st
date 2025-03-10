"
I am an abstract base class for package transformations.

All that I provide is the package name, my subclass transformations are operating on, and a instance creation method for setting the package name and an initial namespace model.
"
Class {
	#name : #RBPackageTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		#packageName,
		#newName
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #accessing }
RBPackageTransformation class >> model: aRBModel packageName: aName [
	^ self new
		model: aRBModel;
		packageName: aName;
		yourself
]

{ #category : #accessing }
RBPackageTransformation class >> packageName: aName [
	^ self new
		packageName: aName
]

{ #category : #executing }
RBPackageTransformation >> packageName [

	^ packageName
]

{ #category : #executing }
RBPackageTransformation >> packageName: anObject [

	packageName := anObject
]

{ #category : #preconditions }
RBPackageTransformation >> preconditions [
	^ (RBCondition withBlock: [ [ RPackage organizer includesPackageNamed: packageName ]
			on: Error
			do: [ :e | self refactoringError: e messageText ]
		])
]
