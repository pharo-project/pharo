"
Removes a class from the image.

Usage:
| transformation |
transformation := (RBRemoveClassTransformation
	className: #RBRemoveClassTransformationTest)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- class must exist
- class to be removed should not be referenced
- class to be removed must not have subclasses
- if it is a trait, it should not be used

"
Class {
	#name : #RBRemoveClassTransformation,
	#superclass : #RBClassTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #private }
RBRemoveClassTransformation class >> cannotRemoveTopLevelClassErrorMesssage [

	^ 'Cannot remove top level class', Character cr asString ,'when it has subclasses'
]

{ #category : #api }
RBRemoveClassTransformation class >> className: aClassName [

	^ self new
		className: aClassName;
		yourself
]

{ #category : #api }
RBRemoveClassTransformation class >> classNames: aCollection [

	^ self 
		model: RBTransformation new model
		classNames: aCollection
]

{ #category : #api }
RBRemoveClassTransformation class >> model: aRBSmalltalk className: aClassName [

	^ (self new)
		model: aRBSmalltalk;
		className: aClassName;
		yourself
]

{ #category : #api }
RBRemoveClassTransformation class >> model: aRBModel classNames: aCollection [

	^ RBCompositeTransformation new
		transformations: (aCollection
			collect: [ :e | self model: aRBModel className: e ]);
		yourself
]

{ #category : #preconditions }
RBRemoveClassTransformation >> environmentWithReferencesTo: aClass [

	^ RBBrowserEnvironment new 
		referencesTo: (self environment 
			associationAt: aClass 
			ifAbsent: [ aClass ])
]

{ #category : #preconditions }
RBRemoveClassTransformation >> environmentWithUsersOf: aTrait [

	^ RBClassEnvironment 
		onEnvironment: RBBrowserEnvironment new
		classes: aTrait realClass users
]

{ #category : #preconditions }
RBRemoveClassTransformation >> preconditionHasNoReferences: aClass [

	| env |
	env := self environmentWithReferencesTo: aClass.
	^ (RBCondition withBlock: [ env isEmpty ])
		errorMacro: aClass , ' is referenced.<n>Browse references?';
		errorBlock: [ self openBrowserOn: (env) ];
		yourself
]

{ #category : #preconditions }
RBRemoveClassTransformation >> preconditionHasNoSubclasses: aClass [

	^ (RBCondition withBlock: [ aClass subclasses isEmpty ])
		errorMacro: self class cannotRemoveTopLevelClassErrorMesssage;
		yourself
]

{ #category : #preconditions }
RBRemoveClassTransformation >> preconditionHasNoUsers: aClassOrTrait [

	^ (RBCondition withBlock: [ aClassOrTrait realClass users isEmpty ])
		errorMacro: aClassOrTrait name , ' Trait is used.<n>Browse classes using it?';
		errorBlock: [ self openBrowserOn: (self environmentWithUsersOf: aClassOrTrait) ];
		yourself
]

{ #category : #preconditions }
RBRemoveClassTransformation >> preconditionIsNotMetaclass: aClass [

	^ ((RBCondition isMetaclass: aClass)
		errorMacro: 'Cannot remove just the metaclass') not
]

{ #category : #preconditions }
RBRemoveClassTransformation >> preconditions [

	^ className inject: RBCondition empty into: [ :cond :aClass |
		| aClassOrTrait |
		aClassOrTrait := self model classNamed: className asSymbol.
		aClassOrTrait ifNil: [ self
			refactoringError: 'No such class or trait named ', className ].
		
		cond 
			& ( (self preconditionIsNotMetaclass: aClassOrTrait) 
			& (self preconditionHasNoReferences: className)
			& (self preconditionHasNoSubclasses: aClassOrTrait)
			& (self preconditionHasNoUsers: aClassOrTrait ) ) ]
]

{ #category : #executing }
RBRemoveClassTransformation >> privateTransform [

	self
		reparentSubclasses;
		removeClass
]

{ #category : #private }
RBRemoveClassTransformation >> removeClass [

	self model removeClassNamed: className
]

{ #category : #private }
RBRemoveClassTransformation >> reparentSubclasses [

	self model 
		reparentClasses: self definingClass subclasses copy 
		to: self definingClass superclass
]

{ #category : #printing }
RBRemoveClassTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]
