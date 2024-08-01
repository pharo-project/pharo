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
	#name : 'RBDeprecateMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'useInsteadSelector',
		'selectorToDeprecate'
	],
	#category : 'Refactoring-UI-Drivers to migrate to new architecture',
	#package : 'Refactoring-UI',
	#tag : 'Drivers to migrate to new architecture'
}

{ #category : 'instance creation' }
RBDeprecateMethodDriver class >> deprecateMethod: aString in: aClass scopes: aCollection [

	^ self new deprecateMethod: aString in: aClass  scopes: aCollection
]

{ #category : 'initialization' }
RBDeprecateMethodDriver >> deprecateMethod: aSelector in: aClass scopes: aCollection [

	scopes := aCollection.
	model := self refactoringScopeOn: scopes first.

	refactoring := RBDeprecateMethodTransformation
		               model: model
		               deprecateMethod: aSelector
		               in: aClass.
	selectorToDeprecate := aSelector.
	self selectorToReplaceDeprecatedSelector.
]

{ #category : 'execution' }
RBDeprecateMethodDriver >> runRefactoring [

	| compositeChange |
	compositeChange := [
	                   [ 
							[ | isValid |
							"the user pressed cancel"
							useInsteadSelector ifNil: [ ^ self ].
							refactoring newSelector: useInsteadSelector.
							isValid := refactoring failedApplicabilityPreconditions isEmpty.
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

{ #category : 'ui - requests' }
RBDeprecateMethodDriver >> selectorToReplaceDeprecatedSelector [

	useInsteadSelector := SpRequestDialog new
		                      title:
			                      'Selector to replace ' , selectorToDeprecate;
		                      label:
			                      'The new selector should not be empty or same as the deprecated one.';
		                      text: selectorToDeprecate;
		                      acceptLabel: 'Accept';
		                      cancelLabel: 'Cancel';
		                      openModal.
	"we should check that the selector is valid here!!!"
]
