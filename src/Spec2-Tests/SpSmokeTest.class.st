"
I am an abstract test for Spec widgets.

I use classToTest to initialize an instance. And I can keep the created windows in a variable that I clean after tests.

I provide openInstance and openInstance: to open it. These methods make me keep the created window in the variable.

- testOpenWithSpec tests a call at openWhitSpec, it test the minimal use but can fail in some widgets needing to be initialized.
- testExample tests the class side method example if exist, example should be a representative use.
"
Class {
	#name : #SpSmokeTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Utils'
}

{ #category : #testing }
SpSmokeTest class >> isAbstract [
	^ self name = #SpSmokeTest
]

{ #category : #testing }
SpSmokeTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #tests }
SpSmokeTest >> testExample [
	"We do not use #shouldnt:raise: to get feedback on the CI"

	self flag: #todo.	"All widgets should have an example. When it will be done remove this skip"
	(self classToTest respondsTo: #example) ifFalse: [ self skip ].

	[ window := self classToTest example ]
		on: Error
		do: [ :e | 
			self
				fail:
					'Broken class example: ' , self classToTest asString
						,
							'
' , e signalerContext shortStack ]
]

{ #category : #tests }
SpSmokeTest >> testOpenWithSpec [
	"We do not use #shouldnt:raise: to get feedback on the CI"

	[ self openInstance ]
		on: Error
		do: [ :e | 
			self
				fail:
					'Broken class simple instance: ' , self classToTest asString
						,
							'
' , e signalerContext shortStack ]
]
