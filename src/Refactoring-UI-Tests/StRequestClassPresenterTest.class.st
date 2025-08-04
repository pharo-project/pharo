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

{ #category : 'tests' }
StRequestClassPresenterTest >> testValidateAnswerBlock [

	self assert: (presenter validateAnswerBlock value: 'ClassName' value: 'PackageName' value: 'PackageTagName')
]

{ #category : 'tests' }
StRequestClassPresenterTest >> testValidateAnswerBlockWithEmptyStrings [

	self deny: (presenter validateAnswerBlock value: '' value: 'PackageName' value: 'PackageTagName').
	self deny: (presenter validateAnswerBlock value: 'ClassName' value: '' value: 'PackageTagName').
	self deny: (presenter validateAnswerBlock value: 'ClassName' value: 'PackageName' value: '')
]

{ #category : 'tests' }
StRequestClassPresenterTest >> testValidateAnswerBlockWithNils [

	self deny: (presenter validateAnswerBlock value: nil value: 'PackageName' value: 'PackageTagName').
	self deny: (presenter validateAnswerBlock value: 'ClassName' value: nil value: 'PackageTagName').
	self deny: (presenter validateAnswerBlock value: 'ClassName' value: 'PackageName' value: nil)
]

{ #category : 'tests' }
StRequestClassPresenterTest >> testValidateAnswerBlockWithSpaces [

	self deny: (presenter validateAnswerBlock value: 'Class Name' value: 'PackageName' value: 'PackageTagName').
	self assert: (presenter validateAnswerBlock value: 'ClassName' value: 'Package Name' value: 'PackageTagName').
	self assert: (presenter validateAnswerBlock value: 'ClassName' value: 'PackageName' value: 'Package Tag Name')
]
