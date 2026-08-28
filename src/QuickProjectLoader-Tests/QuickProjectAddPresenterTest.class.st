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
	presenter useIncomingCheckBox state: true.

	self assert: presenter isFormValid.
	entry := presenter createEntry.

	self assert: entry name equals: 'Roassal'.
	self assert: entry repositoryUrl equals: 'github://ObjectProfile/Roassal/src'.
	self assert: entry baselineName equals: 'Roassal'.
	self assert: entry groups equals: #('Core' 'Tests').
	self assert: entry useIncomingOnConflict
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testCreatesEntryWithOptionalBaselineEmpty [

	| entry |
	presenter nameInput text: 'MyRepo'.
	presenter urlInput text: 'github://owner/repo'.
	presenter baselineInput text: ''.

	self assert: presenter isFormValid.
	entry := presenter createEntry.

	self assert: entry name equals: 'MyRepo'.
	self assert: entry repositoryUrl equals: 'github://owner/repo'.
	self assert: entry baselineName isEmpty
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testPresenterInitializes [

	self assert: presenter nameInput text isEmpty.
	self assert: presenter urlInput text isEmpty.
	self assert: presenter baselineInput text isEmpty.
	self assert: presenter groupsInput text isEmpty.
	self assert: presenter useIncomingCheckBox state
]

{ #category : 'tests' }
QuickProjectAddPresenterTest >> testValidationRejectsEmpty [

	self deny: presenter isFormValid.

	presenter nameInput text: 'Name'.
	self deny: presenter isFormValid.

	presenter urlInput text: 'github://a/b'.
	self assert: presenter isFormValid
]
