"
I represent a bridge between refactoring core and UI. I am abstract class and you should use my subclasses to invoke specific refactorings.

I am a driver object responsible for invoking refactorings. I am responsible for:
- gathering information from user that are needed to perform a refactoring
- checking preconditions and raising an error if they are not fulfilled
- checking braking changes and warning user if they exist
- and finally, invoking refactoring

You can execute me by first creating an instance of one of my subclasses with supplying the browser context:

```
RBMoveMethodToClassSide withContext: context
```

then you can send the `execute` message that will execute the refactoring.

"
Class {
	#name : #RBDriver,
	#superclass : #Object,
	#category : #'Refactoring-Core-UI'
}

{ #category : #testing }
RBDriver class >> isAbstract [
		
	^ self == RBDriver
]

{ #category : #preconditions }
RBDriver >> checkApplicability [

	^ self subclassResponsibility
]

{ #category : #preconditions }
RBDriver >> checkBreakingChanges [

	^ self subclassResponsibility
]

{ #category : #preconditions }
RBDriver >> confirmBreakingChanges [

	self subclassResponsibility
]

{ #category : #execution }
RBDriver >> execute [

	self checkApplicability ifFalse: [ 
		^ self refactoringError ].
	self checkBreakingChanges ifFalse: [
		"Open warning dialog. It throws an error if user answers No."
		self confirmBreakingChanges ].
	self executeRefactoring
]

{ #category : #execution }
RBDriver >> executeRefactoring [
	self subclassResponsibility
]

{ #category : #error }
RBDriver >> refactoringError [

	self subclassResponsibility
]
