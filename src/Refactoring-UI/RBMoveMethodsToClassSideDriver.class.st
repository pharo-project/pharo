"
I represent a driver that invokes `MoveToClassSide` refactoring.

I implement methods from parent class and define some helper methods that are needed to execute `MoveToClassSide` refactoring:
- `generateRefactorings` - creating `MoveToClassSide` refactorings for each of the selected methods
- checking preconditions and breaking changes
- executing the refactoring

You can create my instance and execute the refactoring by running:

```
(RBMoveMethodsToClassSideDriver model: aRBNamespace methods: aCollectionOfMethods) runRefactoring
```
"
Class {
	#name : #RBMoveMethodsToClassSideDriver,
	#superclass : #RBDriver,
	#instVars : [
		'refactorings',
		'methods'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #'instance creation' }
RBMoveMethodsToClassSideDriver class >> model: aNamespace scopes: scopesList methods: aCollectionOfMethods [

	^ self new model: aNamespace scopes: scopesList methods: aCollectionOfMethods
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> displayErrors: errorsList [

	| errorStrings |

	errorStrings := String streamContents: [ :stream |
		                errorsList
			                do: [ :err | stream << err messageText ]
			                separatedBy: [ stream << String cr ] ].
	^ RBRefactoringError signal: errorStrings
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> displayWarnings: errorsList [

	| errorStrings |

	errorStrings := String streamContents: [ :stream |
		                errorsList
			                do: [ :err | stream << err messageText ]
			                separatedBy: [ stream << String cr ] ].
	^ RBRefactoringWarning signal: errorStrings
]

{ #category : #initialization }
RBMoveMethodsToClassSideDriver >> model: aNamespace scopes: scopesList methods: aCollection [

	model := aNamespace.
	scopes := scopesList.
	methods := aCollection.
	refactorings := methods collect: [ :m |
		                RBMoveMethodToClassSideRefactoring
			                model: model
			                method: m
			                class: m origin ]
]

{ #category : #execution }
RBMoveMethodsToClassSideDriver >> runRefactoring [

	| compositeChangesList errors warnings |
	errors := OrderedCollection new.
	warnings := OrderedCollection new.
	compositeChangesList := refactorings collect: [ :refactoring |
		                   [
		                   [ refactoring generateChanges ]
			                   on: RBRefactoringError
			                   do: [ :err |
				                   errors add: err. nil ] ]
			                   on: RBBreakingChangeChecksFailedWarning
			                   do: [ :err |
				                   warnings add: err. nil ] ]
		thenReject: [ :ref | ref isNil ].

	errors ifNotEmpty: [ self displayErrors: errors ].

	warnings ifNotEmpty: [ self displayWarnings: warnings ].

	self openPreviewWithChanges: compositeChangesList
]
