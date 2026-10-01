"
I am the interaction driver that moves a class to another package.

I wrap RBMoveClassTransformation: I ask the user to select a target package, then I run the transformation so the class is re-packaged, showing the repackaging change in the refactoring preview.
"
Class {
	#name : 'ReMoveClassToPackageDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'packageSelectionDialog',
		'targetPackage',
		'targetTag'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReMoveClassToPackageDriver class >> driverName [

	^ 'Move class to package'
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> buildPackageSelectionDialog [

	^ self defaultSelectDialog
		title: 'Move class to package';
		message: 'Choose a target package for ' , class name;
		items: (self candidatePackages sort: #name ascending);
		display: [ :eachPackage | eachPackage name ];
		displayIcon: [ :eachPackage | self application iconNamed: 'package' ];
		yourself
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> candidatePackages [

	^ PackageOrganizer default packages
		  reject: [ :eachPackage | eachPackage name = class packageName ]
]

{ #category : 'execution' }
ReMoveClassToPackageDriver >> gatherUserInput [
	"Ask the user for the target package. Return false if the user cancels."
	| selection |
	
	targetPackage ifNotNil: [
		refactoring
			packageName: targetPackage name;
			tagName: targetTag.
		^ true ].

	selection := self packageSelectionDialog openModal.
	selection ifNil: [ ^ false ].

	refactoring packageName: selection name.

	^ true
]

{ #category : 'configuration' }
ReMoveClassToPackageDriver >> instantiateRefactoring [

	^ RBMoveClassTransformation 
		model: model 
		className: class name
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> packageSelectionDialog [

	^ packageSelectionDialog ifNil: [ 
		packageSelectionDialog := self buildPackageSelectionDialog ]
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> packageSelectionDialog: aDialog [

	packageSelectionDialog := aDialog
]

{ #category : 'initialization' }
ReMoveClassToPackageDriver >> scopes: refactoringScopes class: aClass [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	class := aClass
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> targetPackage: aPackage [

	targetPackage := aPackage
]

{ #category : 'accessing' }
ReMoveClassToPackageDriver >> targetTag: aTagName [

	targetTag := aTagName
]
