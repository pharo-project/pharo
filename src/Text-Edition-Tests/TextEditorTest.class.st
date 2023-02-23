"
Unit test for text editor 
"
Class {
	#name : #TextEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'editor'
	],
	#category : #'Text-Edition-Tests-Editor'
}

{ #category : #'private - accessing' }
TextEditorTest >> createEditor [

	^TextMorph new editor
]

{ #category : #'private - accessing' }
TextEditorTest >> keyboardEvent [
	| event |
	event := KeyboardEvent new.
	event
		setType: #keystroke
		buttons: 0
		position: 0@0
		keyValue: 0
		charCode: 0
		hand: nil
		stamp: Time now.
	^ event
]

{ #category : #running }
TextEditorTest >> setUp [
	super setUp.
	editor := self createEditor
]

{ #category : #running }
TextEditorTest >> tearDown [
	editor := nil.
	super tearDown
]

{ #category : #tests }
TextEditorTest >> testCursorEnd [

	editor cursorEnd: self keyboardEvent
]

{ #category : #tests }
TextEditorTest >> testSetSearchString [

	editor setSearchString
]
