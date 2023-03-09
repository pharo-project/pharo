"
I represent a driver that invokes `PushUpMethod` refactoring.

I am responsible for asking user which methods to push up and to which super class to push it to.
When I gather all needed information I am calling and executing push up method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushUpMethodDriver model: aRBNamespace scopes: refactoringScopes pushUp: methods) execute
```
"
Class {
	#name : #RBPushUpMethodDriver,
	#superclass : #RBDriver,
	#category : #'Refactoring-UI'
}

{ #category : #execution }
RBPushUpMethodDriver >> execute [

	self shouldBeImplemented
]
