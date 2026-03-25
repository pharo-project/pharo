"
A StRequestClassPresenterTest is a test class for testing the behavior of StRequestClassPresenter
"
Class {
	#name : 'StRequestClassPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter'
	],
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'testing' }
StRequestClassPresenterTest class >> isAbstract [

	^ self = StRequestClassPresenterTest
]

{ #category : 'accessing' }
StRequestClassPresenterTest >> newPresenter [

	^ self subclassResponsibility
]

{ #category : 'running' }
StRequestClassPresenterTest >> setUp [
	super setUp.
	presenter := self newPresenter
]
