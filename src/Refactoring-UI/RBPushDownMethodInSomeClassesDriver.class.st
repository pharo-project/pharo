"
I represent a driver that invokes `PushDownMethod` refactoring.

I am responsible for asking user which methods to push down and to which classes to push to.

When I gather all needed information I am calling and executing push down method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushDownMethodDriver model: aRBNamespace scopes: refactoringScopes pushDown: methods) execute
```
"
Class {
	#name : #RBPushDownMethodInSomeClassesDriver,
	#superclass : #RBDriver,
	#instVars : [
		'methods',
		'scopes',
		'class',
		'classes'
	],
	#category : #'Refactoring-UI'
}

{ #category : #initialization }
RBPushDownMethodInSomeClassesDriver class >> model: aRBNamespace scopes: refactoringScopes pushDown: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDown: methods
]

{ #category : #initialization }
RBPushDownMethodInSomeClassesDriver >> model: aRBNamespace scopes: refactoringScopes pushDown: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]
