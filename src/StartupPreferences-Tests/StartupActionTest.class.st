Class {
	#name : 'StartupActionTest',
	#superclass : 'TestCase',
	#instVars : [
		'loader',
		'savedActions',
		'savedErrors'
	],
	#category : 'StartupPreferences-Tests-Startup',
	#package : 'StartupPreferences-Tests',
	#tag : 'Startup'
}

{ #category : 'running' }
StartupActionTest >> brokenAction [

	^ StartupAction
		name: 'Broken action'
		code: '(Smalltalk globals at: #Object) perform: #thisSelectorDoesNotExist'
		runOnce: true
]

{ #category : 'running' }
StartupActionTest >> setUp [
	"#execute writes into the live singleton, whose actions dictionary is part of
	 the running image and holds the user's real startup history. Save and restore
	 it rather than clearing it outright."

	super setUp.
	loader := StartupPreferencesLoader default.
	savedActions := loader actions copy.
	savedErrors := loader errors copy.
	loader cleanSavedActionsAndErrors
]

{ #category : 'running' }
StartupActionTest >> tearDown [

	loader cleanSavedActionsAndErrors.
	"#actions is a Dictionary, so #addAll: needs a keyed collection — passing an
	 Array of associations would make Dictionary>>addAll: iterate index->element."
	loader actions addAll: savedActions.
	loader errors addAll: savedErrors.
	super tearDown
]

{ #category : 'running' }
StartupActionTest >> testBrokenActionDoesNotRaise [
	"#execute does not propagate a failure raised by the action's own code. One bad
	 entry in a shared preferences file would otherwise abort the whole startup
	 sequence (see #19869)."

	self brokenAction execute
]

{ #category : 'running' }
StartupActionTest >> testBrokenActionIsNotMarkedAsExecuted [
	"A failed action stays unexecuted. #add: keys actions by name in a dictionary that
	 is saved with the image and never resets this flag, so an action marked executed
	 is never attempted again -- across image saves, and after the exported file has
	 been corrected."

	| action |
	action := self brokenAction.
	action execute.
	self deny: action hasBeenExecuted
]

{ #category : 'running' }
StartupActionTest >> testBrokenActionIsRecordedInErrors [
	"The failure is recorded in StartupPreferencesLoader default errors, paired with
	 the action that raised it. Swallowing the exception is only acceptable because
	 it stays observable here."

	| action |
	action := self brokenAction.
	action execute.
	self assert: loader errors size equals: 1.
	self assert: loader errors first second identicalTo: action
]

{ #category : 'running' }
StartupActionTest >> testSuccessfulActionIsMarkedAsExecuted [
	"An action whose code completes is marked executed and records no error. With
	 #testBrokenActionIsNotMarkedAsExecuted this pins both outcomes of the
	 #hasBeenExecuted assignment."

	| action executed |
	executed := false.
	action := StartupAction name: 'Working action' code: [ executed := true ] runOnce: true.
	action execute.
	self assert: executed.
	self assert: action hasBeenExecuted.
	self assert: loader errors isEmpty
]
