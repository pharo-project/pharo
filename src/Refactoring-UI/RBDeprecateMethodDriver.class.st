"
I represent a driver that invokes `DeprecateMethod` refactoring.

I am responsible for asking user which selector will replace deprecated method and validating if selected selector can be used.
When I gather all needed information I am calling and executing deprecate method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBDeprecateMethodDriver model: aRBNamespace deprecateMethod: aSelector in: aClass) runRefactoring
```
"
Class {
	#name : #RBDeprecateMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'refactoring',
		'useInsteadSelector',
		'selectorToDeprecate'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #'instance creation' }
RBDeprecateMethodDriver class >> deprecateMethod: aString in: aClass scopes: aCollection [

	^ self new deprecateMethod: aString in: aClass  scopes: aCollection
]

{ #category : #initialization }
RBDeprecateMethodDriver >> deprecateMethod: aSelector in: aClass scopes: aCollection [

	scopes := aCollection.
	model := self refactoringScopeOn: scopes first.

	refactoring := RBDeprecateMethodRefactoring
		               model: model
		               deprecateMethod: aSelector
		               in: aClass.
	selectorToDeprecate := aSelector.
	self selectorToReplaceDeprecatedSelector.
]

{ #category : #accessing }
RBDeprecateMethodDriver >> refactoring [
	^ refactoring
]

{ #category : #execution }
RBDeprecateMethodDriver >> runRefactoring [

	| compositeChange |
	compositeChange := [
	                   [ 
							[ | isValid |
							"the user pressed cancel"
							useInsteadSelector ifNil: [ ^ self ].
							refactoring newSelector: useInsteadSelector.
							isValid := refactoring applicabilityPreconditions check.
							isValid ifFalse: [ self selectorToReplaceDeprecatedSelector ].
							isValid ] whileFalse.
							
							refactoring generateChanges 
							]
		                   on: RBApplicabilityChecksFailedError
		                   do: [ :err |
		                   ^ RBRefactoringError signal: err messageText ] ]
		                   on: RBBreakingChangeChecksFailedWarning
		                   do: [ :err |
			                   RBRefactoringWarning signal: err messageText.
			                   "If user answers no, error is being propagated."
			                   err resume ].
	self openPreviewWithChanges: compositeChange
]

{ #category : #'ui - requests' }
RBDeprecateMethodDriver >> selectorToReplaceDeprecatedSelector [
	"UIManager default
		                      request:
			                      'Method to replace ' , selectorToDeprecate
			                      ,
			                      '. The new selectors cannot be an empty string or same as the old selector.'
		                      initialAnswer: selectorToDeprecate
		                      title: 'New selector'"

	useInsteadSelector := SpRequestDialog new
		                      title:
			                      'Selector to replace ' , selectorToDeprecate;
		                      label:
			                      'The new selectors cannot be an empty string or same as the old selector.';
		                      text: selectorToDeprecate;
		                      acceptLabel: 'Accept';
		                      cancelLabel: 'Cancel';
		                      openModal.
	"we should check that the selector is valid here!!!"
]
