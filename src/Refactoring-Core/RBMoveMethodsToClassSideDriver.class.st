"
I represent a driver that invokes `MoveToClassSide` refactoring.

I implement methods from parent class and define some helper methods that are needed to execute `MoveToClassSide` refactoring:
- `prepareFullExecution` - retrieve all selected methods from context
- `generateRefactorings` - creating `MoveToClassSide` refactorings for each of the methods
- checking preconditions and breaking changes
- executing the refactoring

You can create my instance and execute the refactoring by running:

```
(RBMoveMethodsToClassSideDriver withContext: context) execute
```
"
Class {
	#name : #RBMoveMethodsToClassSideDriver,
	#superclass : #RBDriver,
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'refactorings',
		'methods'
	],
	#category : #'Refactoring-Core-UI'
}

{ #category : #preconditions }
RBMoveMethodsToClassSideDriver >> checkApplicability [

	| checks |
	checks := self refactorings collect: [ :each | 
		          each applicabilityPreconditions check ].
	^ checks reduce: [ :a :b | a & b ]
]

{ #category : #preconditions }
RBMoveMethodsToClassSideDriver >> checkBreakingChanges [

	| checks |
	checks := self refactorings collect: [ :each | 
		          each breakingChangeConditions check ].
	
	^ checks reduce: [ :a :b | a & b ]
]

{ #category : #preconditions }
RBMoveMethodsToClassSideDriver >> confirmBreakingChanges [

	| errorStrings |
	errorStrings := self refactorings
		                reject: [ :each | each breakingChangeConditions check ]
		                thenCollect: [ :each | 
		                each breakingChangeConditions errorString ].
	errorStrings := String streamContents: [ :stream | 
		                errorStrings
			                do: [ :str | stream << str ]
			                separatedBy: [ stream << String cr ] ].
	errorStrings := errorStrings , String cr
	                , ' Method(s) in the class side will be overwritten.'.
	RBRefactoringWarning signal: errorStrings
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> executeRefactoring [

	refactorings do: [ :each | each execute ]
]

{ #category : #accessing }
RBMoveMethodsToClassSideDriver >> generateRefactorings [

	refactorings := methods collect: [ :each | 
		                RBMoveMethodToClassSideRefactoring
			                model: model
			                method: each
			                class: each origin ].
	^ refactorings
]

{ #category : #accessing }
RBMoveMethodsToClassSideDriver >> methods [

	^ methods
]

{ #category : #accessing }
RBMoveMethodsToClassSideDriver >> methods: anObject [

	methods := anObject
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> prepareFullExecution [

	super prepareFullExecution.
	self setUpModelFromContext: self context.
	methods := self context selectedMethods
]

{ #category : #error }
RBMoveMethodsToClassSideDriver >> refactoringError [

	| errorStrings |
	errorStrings := self refactorings
		                reject: [ :each | each applicabilityPreconditions check ]
		                thenCollect: [ :each | 
		                each applicabilityPreconditions errorString ].
	errorStrings := String streamContents: [ :stream | 
		                errorStrings
			                do: [ :str | stream << str ]
			                separatedBy: [ stream << String cr ] ].
	RBRefactoringFailure signal: errorStrings
]

{ #category : #accessing }
RBMoveMethodsToClassSideDriver >> refactorings [

	^ refactorings ifNil: [ self generateRefactorings ]
]
