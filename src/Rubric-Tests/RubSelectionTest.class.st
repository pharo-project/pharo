Class {
	#name : #RubSelectionTest,
	#superclass : #TestCase,
	#instVars : [
		'editor'
	],
	#category : #'Rubric-Tests-Base'
}

{ #category : #running }
RubSelectionTest >> setUp [

	super setUp.
	editor := RubTextEditor forTextArea: RubTextFieldArea new.
	"Add text with three words"
	editor addString: 'abcde fghi jklm'.
]

{ #category : #running }
RubSelectionTest >> testCaretAtBeginningHasNoWordAtCaret [

	editor selectAt: 1.
	
	self assert: editor wordAtCaret isEmpty
]

{ #category : #running }
RubSelectionTest >> testCaretInBetweenTwoWordsHasNoWordAtCaret [

	"Words are abcde fghi. Put caret after space, just before $f"
	editor selectAt: 1 + 'abcde ' size.
	
	self assert: editor wordAtCaret isEmpty
]

{ #category : #running }
RubSelectionTest >> testCaretInTheEndOfSecondWordHasSecondWordUpToCaret [

	"Words are abcde fghi. Put caret after space, just before $i"
	editor selectAt: 1 + 'abcde fghi' size.
	
	self assert: editor wordAtCaret equals: 'fghi'
]

{ #category : #running }
RubSelectionTest >> testCaretInTheEndOfWordHasWordUpToCaret [

	"Word is abcde. Put caret after $e"
	editor selectAt: 1 + 'abcde' size.
	
	self assert: editor wordAtCaret equals: 'abcde'
]

{ #category : #running }
RubSelectionTest >> testCaretInTheMiddleOfSecondWordHasSecondWordUpToCaret [

	"Words are abcde fghi. Put caret after space, just before $i"
	editor selectAt: 1 + 'abcde fgh' size.
	
	self assert: editor wordAtCaret equals: 'fgh'
]

{ #category : #running }
RubSelectionTest >> testCaretInTheMiddleOfWordHasWordUpToCaret [

	"Word is abcde. Put caret after $c"
	editor selectAt: 1 + 'abc' size.
	
	self assert: editor wordAtCaret equals: 'abc'
]
