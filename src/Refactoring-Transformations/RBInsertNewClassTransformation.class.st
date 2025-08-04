"
Adds a class in the image, optionally inside an hierarchy (with superclass or subclasses).

Usage:
```
| transformation |
transformation := RBInsertNewClassTransformation new
	className: #InsertedClass;
	superclass: #RBAbstractRefactoringTest;
	subclasses: (Array with: RBInsertClassParametrizedTest);
	packageName: #'Refactoring2-Transformations-Tests';
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
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
		'packageName',
		'tagName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBInsertNewClassTransformation >> applicabilityPreconditions [
	"Superclass and subclasses shouldn't be metaclasses.
	Each subclass should be immediate subclass of superclass."

	| cond |
	cond := { ((RBCondition isMetaclass:
		          (self model classObjectFor: superclass)) errorMacro:
		         'Superclass must not be a metaclass') not }.
	subclasses do: [ :each |
		cond := cond , { 
			((RBCondition isMetaclass: (self model classObjectFor: each))
				 errorMacro: 'Subclass must <1?not :>be a metaclass') not.
			(RBCondition
				 isImmediateSubclass: (self model classObjectFor: each)
				 of: (self model classObjectFor: superclass)) } ].
	^ cond , {
		  self isValidClassName.
		  self isNotGlobal.
		  (RBCondition isSymbol: packageName).
		  (RBCondition withBlock: [ packageName isNotEmpty ] errorString:
					   'Invalid package name') }
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

{ #category : 'accessing' }
RBInsertNewClassTransformation >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBInsertNewClassTransformation >> packageName: anObject [

	packageName := anObject
]

{ #category : 'executing' }
RBInsertNewClassTransformation >> privateTransform [

	self model
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: (self model classObjectFor: superclass) name;
				name: className;
				package: packageName;
				tag: tagName ];
		reparentClasses: (subclasses collect: [ :subclass | self model classObjectFor: subclass ]) to: (self model classNamed: className asSymbol)
]

{ #category : 'storing' }
RBInsertNewClassTransformation >> storeOn: aStream [

	aStream nextPutAll: '(('.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' className: #';
		nextPutAll: className;
		nextPutAll: ') superclass: '.
	superclass storeOn: aStream.
	aStream nextPutAll: '; subclasses: '.
	subclasses asArray storeOn: aStream.
	aStream nextPutAll: '; packageName: '.
	packageName storeOn: aStream.
	aStream nextPutAll: '; tagName: '.
	tagName storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBInsertNewClassTransformation >> subclasses: aCollection [

	subclasses := aCollection
]

{ #category : 'api' }
RBInsertNewClassTransformation >> superclass: aSuperclass [

	superclass := aSuperclass
]

{ #category : 'accessing' }
RBInsertNewClassTransformation >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBInsertNewClassTransformation >> tagName: anObject [

	tagName := anObject
]
