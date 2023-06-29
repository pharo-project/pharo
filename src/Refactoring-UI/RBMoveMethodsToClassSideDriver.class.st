"
I represent a driver that invokes `MoveToClassSide` refactoring.

I implement methods from parent class and define some helper methods that are needed to execute `MoveToClassSide` refactoring:
- `generateRefactorings` - creating `MoveToClassSide` refactorings for each of the selected methods
- checking preconditions and breaking changes
- executing the refactoring

You can create my instance and execute the refactoring by running:

```
(RBMoveMethodsToClassSideDriver model: aRBNamespace methods: aCollectionOfMethods) execute
```
"
Class {
	#name : #RBMoveMethodsToClassSideDriver,
	#superclass : #RBDriver,
	#instVars : [
		'refactorings',
		'methods'
	],
	#category : #'Refactoring-UI'
}

{ #category : #'instance creation' }
RBMoveMethodsToClassSideDriver class >> model: aNamespace methods: aCollectionOfMethods [

	^ self new model: aNamespace methods: aCollectionOfMethods
]

{ #category : #preconditions }
RBMoveMethodsToClassSideDriver >> checkApplicability [

	| checks |
	checks := self refactorings collect: [ :each |
		          each applicabilityConditions check ].
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
RBMoveMethodsToClassSideDriver >> execute [

	self checkApplicability ifFalse: [ self refactoringError ].
	self checkBreakingChanges ifFalse: [ "Open warning dialog. It throws an error if user answers No."
		self confirmBreakingChanges ].
	self executeRefactorings
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> executeRefactorings [

	refactorings do: [ :each |
		each transform.
		each performChanges ]
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

{ #category : #initialization }
RBMoveMethodsToClassSideDriver >> model: aNamespace methods: aCollection [
	model := aNamespace.
	methods := aCollection
]

{ #category : #error }
RBMoveMethodsToClassSideDriver >> refactoringError [

	| errorStrings |
	errorStrings := self refactorings
		                reject: [ :each | each applicabilityConditions check ]
		                thenCollect: [ :each |
		                each applicabilityConditions errorString ].
	errorStrings := String streamContents: [ :stream |
		                errorStrings
			                do: [ :str | stream << str ]
			                separatedBy: [ stream << String cr ] ].
	RBRefactoringError signal: errorStrings
]

{ #category : #accessing }
RBMoveMethodsToClassSideDriver >> refactorings [

	^ refactorings ifNil: [ self generateRefactorings ]
]
