"
Adds a class in the image, optionally inside an hierarchy (with superclass or subclasses).

Usage:
```
| transformation |
transformation := (RBInsertClassTransformation
	addClass: #InsertedClass
	superclass: #RBTransformationTest
	subclasses: (Array with: RBInsertClassTransformationTest)
	category: #'Refactoring2-Transformations-Tests')
	transform.
(ChangesBrowser changes: transformation model changes changes) open
```

Preconditions:
- superclass must be an existing class
- each subclass also must be an existing class
- name of the class to be added must be non-existent in the system
- name of the category must be a valid one
"
Class {
	#name : #RBInsertNewClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'superclass',
		'subclasses',
		'category'
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #'instance creation' }
RBInsertNewClassTransformation class >> addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [

	^ self new
		addClass: aName
		superclass: aClass
		subclasses: aCollection
		category: aSymbol;
		yourself
]

{ #category : #'instance creation' }
RBInsertNewClassTransformation class >> model: aRBSmalltalk addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [

	^ self new
		model: aRBSmalltalk;
		addClass: aName
			superclass: aClass
			subclasses: aCollection
			category: aSymbol;
		yourself
]

{ #category : #api }
RBInsertNewClassTransformation >> addClass: aName superclass: aClass category: aSymbol [

	self
		addClass: aName
		superclass: aClass asSymbol
		subclasses: (OrderedCollection new)
		category: aSymbol
]

{ #category : #api }
RBInsertNewClassTransformation >> addClass: aName superclass: superclassName subclasses: aCollection category: aSymbol [

	self className: aName.
	superclass := superclassName asSymbol.
	subclasses := aCollection.
	category := aSymbol asSymbol
]

{ #category : #preconditions }
RBInsertNewClassTransformation >> applicabilityConditions [
	"Superclass and subclasses shouldn't be metaclasses.
	Each subclass should be immediate subclass of superclass."

	| cond |
	cond := ((RBCondition isMetaclass:
		          (self model classObjectFor: superclass)) errorMacro:
		         'Superclass must not be a metaclass') not.
	cond := subclasses inject: cond into: [ :sub :each |
		        sub
		        &
			        ((RBCondition isMetaclass:
				          (self model classObjectFor: each)) errorMacro:
				         'Subclass must <1?not :>be a metaclass') not
		        & (RBCondition
				         isImmediateSubclass: (self model classObjectFor: each)
				         of: (self model classObjectFor: superclass)) ].
	^ cond & self isValidClassName & self isNotGlobal
	  & (RBCondition isSymbol: category)
	  & ((RBCondition withBlock: [ category isNotEmpty ]) errorMacro:
			   'Invalid category name')
]

{ #category : #preconditions }
RBInsertNewClassTransformation >> isNotGlobal [

	^ (RBCondition isGlobal: className in: self model) not
]

{ #category : #preconditions }
RBInsertNewClassTransformation >> isValidClassName [

	^ RBCondition isValidClassName: className
]

{ #category : #executing }
RBInsertNewClassTransformation >> privateTransform [

	self model
		defineClass: ('<1p> %<%< #<2s> package: <3p>'
			expandMacrosWith: (self model classObjectFor: superclass)
			with: className
			with: category asString);
		reparentClasses: (subclasses collect: [ :e | self model classObjectFor: e ])
		to: (self model classNamed: className asSymbol)
]

{ #category : #printing }
RBInsertNewClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' addClass: #';
		nextPutAll: className;
		nextPutAll: ' superclass: '.
	superclass storeOn: aStream.
	aStream nextPutAll: ' subclasses: '.
	subclasses asArray storeOn: aStream.
	aStream nextPutAll: ' category: '.
	category storeOn: aStream.
	aStream nextPut: $)
]
