"
Moves a class to a new category or package

Usage:
| transformation |
transformation := (RBMoveClassTransformation
				move: #RBMoveClassTransformation
				to: #'Refactoring2-Refactorings-Tests')
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class must exist
- the category or package must exist
"
Class {
	#name : #RBMoveClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'oldCategory',
		'category'
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #api }
RBMoveClassTransformation class >> model: aRBModel move: aClassName to: aCategoryName [

	^ self new
		model: aRBModel;
		move: aClassName
		to: aCategoryName;
		yourself
]

{ #category : #api }
RBMoveClassTransformation class >> move: aClassName to: aCategoryName [

	^ self new
		move: aClassName
		to: aCategoryName;
		yourself
]

{ #category : #preconditions }
RBMoveClassTransformation >> applicabilityPreconditions [

	^ self classExist & (RBCondition
		   withBlock: [
		   self model environment categories includes: category ]
		   errorString: 'Category named ' , category , ' does not exist')
]

{ #category : #preconditions }
RBMoveClassTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : #api }
RBMoveClassTransformation >> move: aClassName to: aCategoryName [

	self className: aClassName.
	category := aCategoryName
]

{ #category : #executing }
RBMoveClassTransformation >> privateTransform [

	oldCategory := self definingClass category.
	self definingClass category: category.
	self model category: category for: self definingClass
]

{ #category : #printing }
RBMoveClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' move: ''';
		nextPutAll: category;
		nextPutAll: ''' to: ';
		nextPutAll: className.
	aStream nextPut: $)
]
