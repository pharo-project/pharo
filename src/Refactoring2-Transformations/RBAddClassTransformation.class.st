"
Adds a class in the image, optionally inside an hierarchy (with superclass or subclasses).

Usage:
| transformation |
transformation := (RBAddClassTransformation
	addClass: #FooTest
	superclass: #RBTransformationTest
	subclasses: (Array with: RBAddClassTransformationTest)
	category: #'Refactoring2-Transformations-Tests')
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- superclass must be an existing class
- each subclass also must be an existing class
- name of the class to be added must be non-existent in the system
- name of the category must be a valid one
"
Class {
	#name : #RBAddClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'superclass',
		'subclasses',
		'category'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #'instance creation' }
RBAddClassTransformation class >> addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [

	^ self new
		addClass: aName
		superclass: aClass
		subclasses: aCollection
		category: aSymbol;
		yourself
]

{ #category : #'instance creation' }
RBAddClassTransformation class >> model: aRBSmalltalk addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [

	^ self new
		model: aRBSmalltalk;
		addClass: aName
			superclass: aClass
			subclasses: aCollection
			category: aSymbol;
		yourself
]

{ #category : #api }
RBAddClassTransformation >> addClass: aName superclass: aClass category: aSymbol [

	self
		addClass: aName
		superclass: aClass asSymbol
		subclasses: (OrderedCollection new)
		category: aSymbol
]

{ #category : #api }
RBAddClassTransformation >> addClass: aName superclass: superclassName subclasses: aCollection category: aSymbol [

	self className: aName.
	superclass := superclassName asSymbol.
	subclasses := aCollection.
	category := aSymbol asSymbol
]

{ #category : #preconditions }
RBAddClassTransformation >> preconditions [

	| cond |
	cond := ((RBCondition isMetaclass: (self model classObjectFor: superclass)) 
				errorMacro: 'Superclass must not be a metaclass') not.
	cond := subclasses inject: cond into: [:sub :each | 
		sub & ((RBCondition isMetaclass: (self model classObjectFor: each)) 
					errorMacro: 'Subclass must <1?not :>be a metaclass') not 
			 & (RBCondition 
					isImmediateSubclass: (self model classObjectFor: each)
					of: (self model classObjectFor: superclass))].
	^ cond & (RBCondition isValidClassName: className) 
		& (RBCondition isGlobal: className in: self model) not 
			& (RBCondition isSymbol: category) 
			& ((RBCondition withBlock: [category isEmpty not]) 
					errorMacro: 'Invalid category name')
]

{ #category : #executing }
RBAddClassTransformation >> privateTransform [

	self model
		defineClass: ('<1p> subclass: #<2s> instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: <3p>' 
			expandMacrosWith: (self model classObjectFor: superclass)
			with: className
			with: category asString);
		reparentClasses: (subclasses collect: [ :e | self model classObjectFor: e ])
		to: (self model classNamed: className asSymbol)
]

{ #category : #printing }
RBAddClassTransformation >> storeOn: aStream [ 

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
