"
I am a transformation that turns class into abstract class. This operation is a transformation and not a refactoring because we cannot easily validate that the class is actually not used or when used that it is not receiving a message new.
It just adds class side method `isAbstract` to the selected class, and as such cannot break program. 
So it is behavior preserving but we prefer to stress the fact that we have no warranty that the class is really abstract.
"
Class {
	#name : 'RBMakeClassAbstractTransformation',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'targetClass'
	],
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'instance creation' }
RBMakeClassAbstractTransformation class >> classNamed: aClassName [

	^ self new classNamed: aClassName
]

{ #category : 'testing' }
RBMakeClassAbstractTransformation class >> isTransformation [

	^ true
]

{ #category : 'preconditions' }
RBMakeClassAbstractTransformation >> applicabilityPreconditions [
	"Answer a <Collection> of <RBCondition>"

	^ { 
		self preconditionHaveNoReferences
		}
]

{ #category : 'scripting api - conditions' }
RBMakeClassAbstractTransformation >> checkApplicabilityPreconditions [
	"Check a preconditions and raise an error on violations. This method is part of the scripting API since it raises an error."
	
	| failedPreconditions |
	
	failedPreconditions := self failedApplicabilityPreconditions.
	failedPreconditions ifEmpty: [ ^ self ].
	RBRefactoringError signalFor: failedPreconditions
]

{ #category : 'scripting api - conditions' }
RBMakeClassAbstractTransformation >> checkPreconditions [ 

	self checkApplicabilityPreconditions 
]

{ #category : 'instance creation' }
RBMakeClassAbstractTransformation >> classNamed: aClassName [

	className := aClassName.
	targetClass := self model classNamed: aClassName.
]

{ #category : 'preconditions' }
RBMakeClassAbstractTransformation >> preconditionHaveNoReferences [

	^ ReClassesHaveNoReferencesCondition new
		  model: model;
		  classes: { targetClass }
]

{ #category : 'transforming' }
RBMakeClassAbstractTransformation >> privateTransform [

	(RBAddMethodTransformation
		 sourceCode: 'isAbstract

	^ self == ' , className
		 in: targetClass classSide
		 withProtocol: #testing) execute.

]
