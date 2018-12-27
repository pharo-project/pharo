"
SUnit tests for SpecWindow
"
Class {
	#name : #SpecWindowTest,
	#superclass : #TestCase,
	#instVars : [
		'windowPresenter'
	],
	#category : #'Spec-Tests-Core-Support'
}

{ #category : #tests }
SpecWindowTest >> testAboutText [
	windowPresenter := WindowPresenter new.
	windowPresenter aboutText: 'test'.
	self assert: windowPresenter aboutText equals: 'test'.
	windowPresenter close.
	
	windowPresenter := TextPresenter new aboutText: 'test2'.
	self assert: windowPresenter aboutText equals: 'test2'
]

{ #category : #tests }
SpecWindowTest >> testIsDisplayed [
	"Test for case: 16800 -> ask a SpecWindow for #isDisplayed always returns true"
	
	windowPresenter := TextPresenter new openWithSpec.
	self assert: (windowPresenter isDisplayed).
	
	windowPresenter window minimize.	
	self assert: (windowPresenter isDisplayed).
		
	windowPresenter window close.
	self assert: (windowPresenter isDisplayed not).

	
]

{ #category : #tests }
SpecWindowTest >> testTitle [
	windowPresenter := TextPresenter new openWithSpec.
	
	self assert: windowPresenter isDisplayed.
	self assert: windowPresenter title equals: 'Text'.
	
	windowPresenter title: 'Test Window'.
	self assert: windowPresenter title equals: 'Test Window'.
	windowPresenter close
]
