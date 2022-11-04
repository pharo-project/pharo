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

{ #category : #'ui - requests' }
RBDeprecateMethodDriver >> getMethodThatWillReplaceDeprecatedMethod [

	| newSelector |
	newSelector := UIManager default
		               request: 'Method to replace ' , methodToDeprecate
		               initialAnswer: methodToDeprecate
		               title: 'New selector'.
	newSelector isEmptyOrNil | (newSelector = methodToDeprecate) ifTrue: [ 
		CmdCommandAborted signal ].
	(methodToDeprecate numArgs = newSelector numArgs or: [ 
		 newSelector numArgs = 0 ]) ifFalse: [ 
		RBRefactoringFailure signal:
			'The new selectors should have the same number of arguments as the old one, or not have them at all' ].
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
