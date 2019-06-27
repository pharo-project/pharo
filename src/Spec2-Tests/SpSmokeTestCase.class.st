"
I am an abstract test for Spec widgets.

I use classToTest to initialize an instance. And I can keep the created windows in a variable that I clean after tests.

I provide openInstance and openInstance: to open it. These methods make me keep the created window in the variable.

- testOpenWithSpec tests a call at openWhitSpec, it test the minimal use but can fail in some widgets needing to be initialized.
- testExample tests the class side method example if exist, example should be a representative use.
"
Class {
	#name : #SpSmokeTestCase,
	#superclass : #SpTestCase,
	#category : #'Spec2-Tests-Utils'
}

{ #category : #testing }
SpSmokeTestCase class >> isAbstract [
	^ self name = #SpSmokeTestCase
]

{ #category : #testing }
SpSmokeTestCase class >> shouldInheritSelectors [
	^ true
]

{ #category : #tests }
SpSmokeTestCase >> testExample [
	self flag: #todo.	"All widgets should have an example. When it will be done remove this skip"
	(self classToTest respondsTo: #example) ifFalse: [ self skip ].
	
	self shouldnt: [ window := self classToTest example ] raise: Error
]

{ #category : #tests }
SpSmokeTestCase >> testOpenWithSpec [
	self shouldnt: [ self openInstance ] raise: Error
]
