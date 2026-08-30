"
I am a refactoring for creating a new trait.

My preconditions verify that the trait name and package name are valid and
that the trait name is not already used as a global variable.
"
Class {
	#name : 'RBInsertNewTraitRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'comment',
		'packageName',
		'tagName',
		'traitComposition'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RBInsertNewTraitRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidClassName: className).
		  (RBCondition isGlobal: className in: self model) not.
		  (RBCondition isSymbol: packageName).
		  ((RBCondition withBlock: [ packageName isNotEmpty ]) errorMacro:
			   'Invalid package name') }
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> comment [

	^ comment
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> comment: aString [

	comment := aString
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> packageName: aSymbol [

	packageName := aSymbol
]

{ #category : 'transforming' }
RBInsertNewTraitRefactoring >> privateTransform [

	self model defineClass: [ :aBuilder |
		aBuilder
			beTrait;
			name: className;
			package: packageName;
			tag: tagName;
			comment: self comment;
			traitComposition: (traitComposition ifNil: [ #(  ) ]) ]
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> tagName: aString [

	tagName := aString
]

{ #category : 'accessing' }
RBInsertNewTraitRefactoring >> traitComposition: aTraitComposition [

	traitComposition := aTraitComposition
]
