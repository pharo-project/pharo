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
	#name : 'RBInsertNewClassTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'superclass',
		'subclasses',
		'category'
	],
	#category : 'Refactoring2-Transformations-Model-Unused',
	#package : 'Refactoring2-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBInsertNewClassTransformation >> applicabilityPreconditions [
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

{ #category : 'accessing' }
RBInsertNewClassTransformation >> category [

	^ category
]

{ #category : 'accessing' }
RBInsertNewClassTransformation >> category: anObject [

	category := anObject
]

{ #category : 'initialization' }
RBInsertNewClassTransformation >> initialize [

	super initialize.
	subclasses := #(  )
]

{ #category : 'preconditions' }
RBInsertNewClassTransformation >> isNotGlobal [

	^ (RBCondition isGlobal: className in: self model) not
]

{ #category : 'preconditions' }
RBInsertNewClassTransformation >> isValidClassName [

	^ RBCondition isValidClassName: className
]

{ #category : 'executing' }
RBInsertNewClassTransformation >> privateTransform [

	self model
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: (self model classObjectFor: superclass) name;
				name: className;
				package: category ];
		reparentClasses: (subclasses collect: [ :subclass | self model classObjectFor: subclass ]) to: (self model classNamed: className asSymbol)
]

{ #category : 'printing' }
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

{ #category : 'api' }
RBInsertNewClassTransformation >> subclasses: aCollection [

	subclasses := aCollection
]

{ #category : 'api' }
RBInsertNewClassTransformation >> superclass: superclassName [

	superclass := superclassName asSymbol
]
