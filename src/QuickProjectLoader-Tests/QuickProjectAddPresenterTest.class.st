"
Unit tests for QuickProjectAddPresenter dialog.
"
Class {
	#name : 'QuickProjectAddPresenterTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'running' }
QuickProjectAddPresenterTest >> setUp [

	super setUp.
	presenter := QuickProjectAddPresenter new
]

{ #category : 'running' }
QuickProjectAddPresenterTest >> tearDown [

	presenter window ifNotNil: [ :w | w close ].
	super tearDown
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testCreatesEntryOnAccept [

	| entry |
	presenter nameInput text: 'Roassal'.
	presenter urlInput text: 'github://ObjectProfile/Roassal/src'.
	presenter baselineInput text: 'Roassal'.
	presenter groupsInput text: 'Core, Tests'.

	self assert: presenter isFormValid.
	entry := presenter createEntry.

	self assert: entry name equals: 'Roassal'.
	self assert: entry repositoryUrl equals: 'github://ObjectProfile/Roassal/src'.
	self assert: entry baselineName equals: 'Roassal'.
	self assert: entry groups equals: #('Core' 'Tests')
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testPresenterInitializes [

	self assert: presenter nameInput text isEmpty.
	self assert: presenter urlInput text isEmpty.
	self assert: presenter baselineInput text isEmpty.
	self assert: presenter groupsInput text isEmpty.
	self deny: presenter acceptButton isEnabled
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testValidationRejectsEmpty [

	self deny: presenter isFormValid.
	self deny: presenter acceptButton isEnabled.

	presenter nameInput text: 'Name'.
	self deny: presenter isFormValid.

	presenter urlInput text: 'github://a/b'.
	self deny: presenter isFormValid.

	presenter baselineInput text: 'Baseline'.
	self assert: presenter isFormValid.
	self assert: presenter acceptButton isEnabled
]
