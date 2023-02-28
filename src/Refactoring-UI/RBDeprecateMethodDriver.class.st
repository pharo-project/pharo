"
I represent a driver that invokes `DeprecateMethod` refactoring.

I am responsible for asking user which selector will replace deprecated method and validating if selected selector can be used.
When I gather all needed information I am calling and executing deprecate method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBDeprecateMethodDriver model: aRBNamespace deprecateMethod: aSelector in: aClass) execute
```
"
Class {
	#name : #RBDeprecateMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'methodToDeprecate',
		'methodToReplaceDeprecatedMethod',
		'refactoring',
		'scopes'
	],
	#category : #'Refactoring-UI'
}

{ #category : #'instance creation' }
RBDeprecateMethodDriver class >> deprecateMethod: aString in: aClass scopes: aCollection [

	^ self new deprecateMethod: aString in: aClass  scopes: aCollection
]

{ #category : #initialization }
RBDeprecateMethodDriver >> deprecateMethod: aSelector in: aClass scopes: aCollection [

	methodToDeprecate := aSelector.
	methodToReplaceDeprecatedMethod := self getMethodThatWillReplaceDeprecatedMethod.
	scopes := aCollection.
	model := self refactoringScopeOn: scopes first.

	refactoring := RBDeprecateMethodRefactoring
		               model: model
		               deprecateMethod: aSelector
		               in: aClass
		               using: methodToReplaceDeprecatedMethod
]

{ #category : #execution }
RBDeprecateMethodDriver >> execute [

	| compositeChange |
	compositeChange := [
	                   [
	                   [ refactoring generateChanges ]
		                   on: RBInputValidationFailedError
		                   do: [ :err |
			                   methodToReplaceDeprecatedMethod := self getMethodThatWillReplaceDeprecatedMethod.
			                   refactoring newSelector:
				                   methodToReplaceDeprecatedMethod.
									err resume ] ]
		                   on: RBApplicabilityChecksFailedError
		                   do: [ :err |
		                   ^ RBRefactoringFailure signal: err messageText ] ]
		                   on: RBBreakingChangeChecksFailedWarning
		                   do: [ :err |
			                   RBRefactoringWarning signal: err messageText.
			                   "If user answers no, error is being propagated."
			                   err resume ].

	(StRefactoringPreviewPresenter
		 changes: compositeChange
		 inEnvironment: model
		 scopes: scopes) open
]

{ #category : #'ui - requests' }
RBDeprecateMethodDriver >> getMethodThatWillReplaceDeprecatedMethod [

	| newSelector |
	newSelector := UIManager default
		               request: 'Method to replace ' , methodToDeprecate , '. The new selectors cannot be an empty string or same as the old selector.'
		               initialAnswer: methodToDeprecate
		               title: 'New selector'.
	^ newSelector
]

{ #category : #utilities }
RBDeprecateMethodDriver >> refactoringScopeOn: aScope [

	^ RBClassModelFactory rbNamespace onEnvironment: aScope asRBEnvironment
]
