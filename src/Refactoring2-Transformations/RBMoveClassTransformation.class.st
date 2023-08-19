"
Moves a class to a new package or package tag

Usage:

```st
| transformation |
transformation := (RBMoveClassTransformation
	move: #RBMoveClassTransformation
	toPackage: #'Refactoring2-Refactorings-Tests'
	inTag: #Utilities) ""The tag is optional""
	transform.
(ChangesBrowser changes: transformation model changes changes) open
```

Preconditions:
- the class must exist
- the package and optional tag must exist
"
Class {
	#name : #RBMoveClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'packageName',
		'tagName'
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #api }
RBMoveClassTransformation class >> move: aClassName toPackage: aPackageName [

	^ self new
		  className: aClassName;
		  packageName: aPackageName;
		  yourself
]

{ #category : #api }
RBMoveClassTransformation class >> move: aClassName toPackage: aPackageName inTag: aTagName [

	^ (self move: aClassName toPackage: aPackageName)
		  tagName: aTagName;
		  yourself
]

{ #category : #preconditions }
RBMoveClassTransformation >> applicabilityPreconditions [

	^ self classExist & (RBCondition
		   withBlock: [
			   (self model environment packageAt: self packageName ifAbsent: [ nil ])
				   ifNil: [ false ]
				   ifNotNil: [ :package | self tagName ifNotNil: [ :name | package hasTag: name ] ] ]
		   errorString: 'No package named ' , self packageName , (self tagName
				    ifNil: [ '' ]
				    ifNotNil: [ :name | ' with tag named ' , name ]))
]

{ #category : #preconditions }
RBMoveClassTransformation >> category [

	^ self tagName
		  ifNil: [ self packageName ]
		  ifNotNil: [ self packageName , '-' , tagName ]
]

{ #category : #preconditions }
RBMoveClassTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString:
		  ('Class named <1s> does not exist' expandMacrosWith: className)
]

{ #category : #accessing }
RBMoveClassTransformation >> packageName [

	^ packageName
]

{ #category : #accessing }
RBMoveClassTransformation >> packageName: anObject [

	packageName := anObject
]

{ #category : #executing }
RBMoveClassTransformation >> privateTransform [

	self flag: #package. "This should use the package and tag API."
	self definingClass category: self category.
	self model category: self category for: self definingClass
]

{ #category : #printing }
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

{ #category : #accessing }
RBMoveClassTransformation >> tagName [

	^ tagName
]

{ #category : #accessing }
RBMoveClassTransformation >> tagName: anObject [

	tagName := anObject
]
