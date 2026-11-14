"
I am the abstract superclass of refactorings that create a new class or trait.

I hold what both need: the package, tag, comment and trait composition of the new behavior.
My preconditions verify that the name is a valid class name that is not yet a global,
and that the package name is a non-empty symbol.
"
Class {
	#name : 'RBInsertNewBehaviorRefactoring',
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

{ #category : 'testing' }
RBInsertNewBehaviorRefactoring class >> isAbstract [

	^ self == RBInsertNewBehaviorRefactoring
]

{ #category : 'preconditions' }
RBInsertNewBehaviorRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidClassName: className).
		  (RBCondition isGlobal: className in: self model) not.
		  (RBCondition isSymbol: packageName).
		  ((RBCondition withBlock: [ packageName isNotEmpty ]) errorMacro:
			   'Invalid package name') }
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> comment [

	^ comment
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> comment: aString [

	comment := aString
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> packageName: aSymbol [

	packageName := aSymbol
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> tagName: aString [

	tagName := aString
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> traitComposition [

	^ traitComposition ifNil: [ #(  ) ]
]

{ #category : 'accessing' }
RBInsertNewBehaviorRefactoring >> traitComposition: aTraitComposition [

	traitComposition := aTraitComposition
]
