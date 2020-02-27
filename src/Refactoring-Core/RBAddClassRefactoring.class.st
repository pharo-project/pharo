"
I am a refactoring for creating new classes. 

You can define the name, superclass, category and subclasses.

I am used by other refactorings that may create new classes, for example, RBSplitClassRefactoring.

My preconditions verify that I use a valid class name, that does not yet exists as a global variable, 
and the subclasses (if any) were direct subclasses of the superclass.
"
Class {
	#name : #RBAddClassRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'category',
		'superclass',
		'subclasses'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBAddClassRefactoring class >> addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [
	^self new
		addClass: aName
		superclass: aClass
		subclasses: aCollection
		category: aSymbol
]

{ #category : #'instance creation' }
RBAddClassRefactoring class >> model: aRBSmalltalk addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [ 
	^(self new)
		model: aRBSmalltalk;
		addClass: aName
			superclass: aClass
			subclasses: aCollection
			category: aSymbol;
		yourself
]

{ #category : #initialization }
RBAddClassRefactoring >> addClass: aName superclass: aClass subclasses: aCollection category: aSymbol [ 
	self className: aName.
	superclass := self classObjectFor: aClass.
	subclasses := aCollection collect: [:each | self classObjectFor: each].
	category := aSymbol
]

{ #category : #preconditions }
RBAddClassRefactoring >> preconditions [
	| cond |
	cond := ((RBCondition isMetaclass: superclass) 
				errorMacro: 'Superclass must not be a metaclass') not.
	cond := subclasses inject: cond
				into: 
					[:sub :each | 
					sub 
						& ((RBCondition isMetaclass: each) 
								errorMacro: 'Subclass must <1?not :>be a metaclass') not 
							& (RBCondition isImmediateSubclass: each of: superclass)].
	^cond & (RBCondition isValidClassName: className) 
		& (RBCondition isGlobal: className in: self model) not 
			& (RBCondition isSymbol: category) 
			& ((RBCondition withBlock: [category isNotEmpty ]) 
					errorMacro: 'Invalid category name')
]

{ #category : #printing }
RBAddClassRefactoring >> storeOn: aStream [ 
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

{ #category : #transforming }
RBAddClassRefactoring >> transform [
	(self model)
		defineClass: ('<1p> subclass: #<2s> instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: <3p>' 
					expandMacrosWith: superclass
					with: className
					with: category asString);
		reparentClasses: subclasses to: (self model classNamed: className asSymbol)
]
