"
Moves a class to a new package or package tag

Usage:

```st
| transformation |
transformation := (RBMoveClassTransformation
	move: #RBMoveClassTransformation
	toPackage: #'Refactoring-Transformations-Tests'
	inTag: #Utilities) ""The tag is optional""
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the class must exist
- the package and optional tag must exist
"
Class {
	#name : 'RBMoveClassTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'packageName',
		'tagName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBMoveClassTransformation class >> move: aClassName toPackage: aPackageName [

	^ self new
		  className: aClassName;
		  packageName: aPackageName;
		  yourself
]

{ #category : 'api' }
RBMoveClassTransformation class >> move: aClassName toPackage: aPackageName inTag: aTagName [

	^ (self move: aClassName toPackage: aPackageName)
		  tagName: aTagName;
		  yourself
]

{ #category : 'preconditions' }
RBMoveClassTransformation >> applicabilityPreconditions [

	^ {
		  self classExist.
		  (RBCondition
			   withBlock: [
				   (self model environment
					    packageAt: self packageName
					    ifAbsent: [ nil ]) ifNil: [ false ] ifNotNil: [ :package |
					   self tagName ifNotNil: [ :name | package hasTag: name ].
					   true ] ]
			   errorString:
				   'No package named ' , self packageName , (self tagName
					    ifNil: [ '' ]
					    ifNotNil: [ :name | ' with tag named ' , name ])) }
]

{ #category : 'scripting api - conditions' }
RBMoveClassTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBMoveClassTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : 'accessing' }
RBMoveClassTransformation >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBMoveClassTransformation >> packageName: anObject [

	packageName := anObject
]

{ #category : 'executing' }
RBMoveClassTransformation >> privateTransform [

	self definingClass
		packageName: self packageName;
		tagName: self tagName.
	self model repackage: self definingClass in: self packageName tag: self tagName
]

{ #category : 'storing' }
RBMoveClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' move: ''';
		nextPutAll: className;
		nextPutAll: ''' toPackage: ''';
		nextPutAll: packageName;
		nextPutAll: ''''.
	self tagName ifNotNil: [
		aStream
			nextPutAll: ' inTag: ''';
			nextPutAll: tagName;
			nextPutAll: '''' ].
	aStream nextPut: $)
]

{ #category : 'accessing' }
RBMoveClassTransformation >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBMoveClassTransformation >> tagName: anObject [

	tagName := anObject
]
