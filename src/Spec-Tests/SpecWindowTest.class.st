"
SUnit tests for SpecWindow
"
Class {
	#name : #SpecWindowTest,
	#superclass : #TestCase,
	#category : #'Spec-Tests-Core-Support'
}

{ #category : #tests }
SpecWindowTest >> testIsDisplayed [

	"Test for case: 16800 -> ask a SpecWindow for #isDisplayed always returns true"
	
	| windowPresenter |

	windowPresenter := TextPresenter new openWithSpec.	
	self assert: (windowPresenter isDisplayed).
	
	windowPresenter window minimize.	
	self assert: (windowPresenter isDisplayed).
		
	windowPresenter window close.
	self assert: (windowPresenter isDisplayed not).

	
]
