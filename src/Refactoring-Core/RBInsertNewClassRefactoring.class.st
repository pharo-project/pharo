"
I am a refactoring for creating new classes. 

You can define the name, superclass, category and subclasses.

I am used by other refactorings that may create new classes, for example, RBSplitClassRefactoring.

My preconditions verify that I use a valid class name, that does not yet exists as a global variable, 
and the subclasses (if any) were direct subclasses of the superclass.
"
Class {
	#name : 'RBInsertNewClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'category',
		'superclass',
		'subclasses',
		'comment'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> category [

	^ category
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> category: anObject [

	category := anObject
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> comment [

	^ comment
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> comment: aString [
	comment := aString
]

{ #category : 'initialization' }
RBInsertNewClassRefactoring >> initialize [

	super initialize.
	subclasses := #(  )
]

{ #category : 'preconditions' }
RBInsertNewClassRefactoring >> preconditions [

	| cond |
	cond := ((RBCondition isMetaclass: superclass) errorMacro: 'Superclass must not be a metaclass') not.
	cond := subclasses inject: cond into: [ :sub :each |
		        sub & ((RBCondition isMetaclass: each) errorMacro: 'Subclass must <1?not :>be a metaclass') not
		        & (RBCondition isImmediateSubclass: each of: superclass) ].
	^ cond & (RBCondition isValidClassName: className) & (RBCondition isGlobal: className in: self model) not & (RBCondition isSymbol: category)
	  & ((RBCondition withBlock: [ category isNotEmpty ]) errorMacro: 'Invalid category name')
]

{ #category : 'transforming' }
RBInsertNewClassRefactoring >> privateTransform [

	self model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: superclass;
				name: className;
				package: self category;
				comment: self comment ];
		reparentClasses: subclasses to: (self model classNamed: className)
]

{ #category : 'printing' }
RBInsertNewClassRefactoring >> storeOn: aStream [
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

{ #category : 'initialization' }
RBInsertNewClassRefactoring >> subclasses: aCollection [

	subclasses := aCollection collect: [ :each | self classObjectFor: each ]
]

{ #category : 'initialization' }
RBInsertNewClassRefactoring >> superclass: aClass [

	superclass := self classObjectFor: aClass
]
