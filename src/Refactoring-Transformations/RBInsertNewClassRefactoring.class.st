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
	#superclass : 'RBInsertNewBehaviorRefactoring',
	#instVars : [
		'superclass',
		'subclasses'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
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
	^ cond , super applicabilityPreconditions
]

{ #category : 'transforming' }
RBInsertNewClassRefactoring >> checkSuperclass [

	| superclassObj superclassName |
	"I fail if superclass is not defined in the model"
	superclassObj := self model classObjectFor: superclass.
	superclassObj ifNil: [
			superclassName := superclass isString
				                  ifTrue: [ superclass , ' ' ]
				                  ifFalse: [ '' ].
			self refactoringError: 'The new class'' superclass ' , superclassName , 'is missing from the model' ]
]

{ #category : 'initialization' }
RBInsertNewClassRefactoring >> initialize [

	super initialize.
	subclasses := #(  )
]

{ #category : 'transforming' }
RBInsertNewClassRefactoring >> prepareForExecution [

	self checkSuperclass
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
				comment: self comment ]
		traitComposition: self traitComposition;
		reparentClasses: subclasses to: (self model classNamed: className)
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> subclasses: aCollection [
	"Set class objects for a list of class names"

	subclasses := aCollection collect: [ :each | self classObjectFor: each ].
]

{ #category : 'accessing' }
RBInsertNewClassRefactoring >> superclass: aClass [
	"set the superclass object"

	superclass := self classObjectFor: aClass
]
