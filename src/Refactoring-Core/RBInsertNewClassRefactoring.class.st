"
I am a refactoring for creating new classes. 

You can define the name, superclass, package/tag and subclasses.

I am used by other refactorings that may create new classes, for example, `RBSplitClassRefactoring`.

My preconditions verify that I use a valid class name, that does not yet exists as a global variable, 
and the subclasses (if any) were direct subclasses of the superclass.

-  If it receives the `subclasses:` message, then this refactoring acts as in ""insertion mode"", assuming that the new class will be inserted between the parent class and direct subclasses of the superclass.

-  If it does not receive the `subclasses:` message, it will act as if it were in ""add mode"" without reparenting the subclasses to the new class.

"
Class {
	#name : 'RBInsertNewClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'superclass',
		'subclasses',
		'comment',
		'packageName',
		'tagName',
		'traitComposition'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBInsertNewClassRefactoring >> applicabilityPreconditions [

	| cond |
	cond := { ((RBCondition isMetaclass: superclass) errorMacro:
		         'Superclass must not be a metaclass') not }.
	subclasses do: [ :sub |
		        cond := cond , {
				 	((RBCondition isMetaclass: sub) errorMacro:
				         'Subclass must <1?not :>be a metaclass') not.
			        (RBCondition isImmediateSubclass: sub of: superclass) } ].
	^ cond , {
			(RBCondition isValidClassName: className).
			(RBCondition isGlobal: className in: self model) not.
			(RBCondition isSymbol: packageName).
			((RBCondition withBlock: [ packageName isNotEmpty ]) errorMacro:
				 'Invalid package name') }
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

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> packageName: anObject [

	packageName := anObject
]

{ #category : 'transforming' }
RBInsertNewClassRefactoring >> privateTransform [

	self model
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: superclass name;
				name: className;
				package: packageName;
				tag: tagName;
				comment: self comment;
				traitComposition: (traitComposition ifNil: #()) ];
		reparentClasses: subclasses to: (self model classNamed: className)
]

{ #category : 'storing' }
RBInsertNewClassRefactoring >> storeOn: aStream [

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

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> subclasses: aCollection [

	subclasses := aCollection collect: [ :each | self classObjectFor: each ].
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> superclass: aClass [

	superclass := self classObjectFor: aClass
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> tagName: anObject [

	tagName := anObject
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> traitComposition: aTraitComposition [

	traitComposition := aTraitComposition
]
