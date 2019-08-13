"
SUnit tests for Button model
"
Class {
	#name : #ButtonPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ButtonPresenterTest >> classToTest [
^ ButtonPresenter
]

{ #category : #running }
ButtonPresenterTest >> morph [
	^ testedInstance widget widget
]

{ #category : #tests }
ButtonPresenterTest >> testAskBeforeChanging [
	| state |
	self assert: testedInstance askBeforeChanging not.
	self openInstance.
	state := testedInstance widget askBeforeChanging.
	self assert: state not.
	testedInstance askBeforeChanging: true.
	state := testedInstance widget askBeforeChanging.
	self assert: state
]

{ #category : #tests }
ButtonPresenterTest >> testEnabled [
	self assert: testedInstance enabled.
	self openInstance.
	self assert: self morph enabled.
	testedInstance enabled: false.
	self deny: self morph enabled
]
