"
I am an abstract test for Spec widgets.

I use classToTest to initialize an instance. And I can keep the created windows in a variable that I clean after tests.

I provide openInstance and openInstance: to open it. These methods make me keep the created window in the variable.

- testOpenWithSpec tests a call at openWhitSpec, it test the minimal use but can fail in some widgets needing to be initialized.
- testExample tests the class side method example if exist, example should be a representative use.
"
Class {
	#name : #SpecSmokeTestCase,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Utils'
}

{ #category : #testing }
SpecSmokeTestCase class >> isAbstract [
	^ self name = #SpecSmokeTestCase
]

{ #category : #testing }
SpecSmokeTestCase class >> shouldInheritSelectors [
	^ true
]

{ #category : #tests }
SpecSmokeTestCase >> testExample [
	self flag: #todo.	"All widgets should have an example. When it will be done remove this skip"
	(self classToTest respondsTo: #example) ifFalse: [ self skip ].
	
	self shouldnt: [ window := self classToTest example ] raise: Error
]

{ #category : #tests }
SpecSmokeTestCase >> testOpenWithSpec [
	self shouldnt: [ self openInstance ] raise: Error
]
