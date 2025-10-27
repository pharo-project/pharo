Class {
	#name : 'RubFindReplaceServiceTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests-Base',
	#package : 'Rubric-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
RubFindReplaceServiceTest >> defaultFindReplaceServiceClass [

	^ SpRubFindReplaceService
]

{ #category : 'tests' }
RubFindReplaceServiceTest >> testCaseSensitive [

	self deny: ( self defaultFindReplaceServiceClass newFor: self) caseSensitive
]

{ #category : 'tests' }
RubFindReplaceServiceTest >> testOpenDialogWithDifferentText [
	"Test that dialog text gets updated when findText differs from dialog input"

	| service mock |
	mock := MockObject new.
	mock on: #announcer respond: Announcer new.
	service := self defaultFindReplaceServiceClass newFor: self.
	service textArea: mock.
	service findText: 'old cached text'.
	service openDialog.
	service dialogWindow close.
	service findText: 'selected text'.
	service openDialog.

	self assert: service dialog findInput text equals: 'selected text'.
	service dialogWindow close
]

{ #category : 'tests' }
RubFindReplaceServiceTest >> testOpenDialogWithSameText [
	"Test that dialog text stays the same when findText matches cached text"

	| service mock text|
	text:= 'same text'.
	mock := MockObject new.
	mock on: #announcer respond: Announcer new.
	service := self defaultFindReplaceServiceClass newFor: self.
	service textArea: mock.
	service findText: text.
	service openDialog.
	"We simulate the find action there"
	service dialog textToFind: text.
	service dialogWindow close.
	"Opening new dialog"
	service openDialog.
	self assert: service dialog findInput text equals: text.
	service dialogWindow close
]
