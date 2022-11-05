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
		'refactoring'
	],
	#category : #'Refactoring-Core-UI'
}

{ #category : #'instance creation' }
RBDeprecateMethodDriver class >> model: aRBNamespace deprecateMethod: aString in: aClass [

	^ self new model: aRBNamespace deprecateMethod: aString in: aClass
]

{ #category : #execution }
RBDeprecateMethodDriver >> execute [

	| changes |
	changes := [ 
	           [ refactoring generateChanges ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err | 
		           ^ RBRefactoringFailure signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err | 
			           RBRefactoringWarning signal: err messageText.
			           "If user answers no, error is being propagated."
			           err resume ].
	self flag: #TODO. "preview changes and confirm before executing"
	refactoring performChanges
]

{ #category : #'ui - requests' }
RBDeprecateMethodDriver >> getMethodThatWillReplaceDeprecatedMethod [

	| newSelector |
	newSelector := UIManager default
		               request: 'Method to replace ' , methodToDeprecate
		               initialAnswer: methodToDeprecate
		               title: 'New selector'.
	newSelector isEmptyOrNil | (newSelector = methodToDeprecate) ifTrue: [ 
		CmdCommandAborted signal ].
	^ newSelector
]

{ #category : #initialization }
RBDeprecateMethodDriver >> model: aRBNamespace deprecateMethod: aSelector in: aClass [

	methodToDeprecate := aSelector.
	methodToReplaceDeprecatedMethod := self getMethodThatWillReplaceDeprecatedMethod.

	refactoring := RBDeprecateMethodRefactoring
		               model: aRBNamespace
		               deprecateMethod: aSelector
		               in: aClass
		               using: methodToReplaceDeprecatedMethod
]
