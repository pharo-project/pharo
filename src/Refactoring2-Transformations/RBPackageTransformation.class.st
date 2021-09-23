Class {
	#name : #RBPackageTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		'packageName',
		'newName',
		'environment'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #'instance creation' }
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

{ #category : #preconditions }
RBPackageTransformation >> packageName [

	^ packageName
]

{ #category : #preconditions }
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
