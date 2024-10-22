Class {
	#name : 'RubTextEditorLocalHistoryTest',
	#superclass : 'RubAbstractTest',
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> expectText: aString [ 

	self assert: editor text asString equals: aString
]

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> selectAt: anIndex [
	editor selectFrom: anIndex to: anIndex - 1
]

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> testRedoCompletionEntryKeepsFollowingLine [

	"If the caret is at the end of a word, replace the entire word"
	editor addString: 'self'.
	editor closeTypeIn.
	editor unselect.
	"Simulate an enter"
	editor crWithIndent: KeyboardEvent new.
	editor addString: '	b'.
	editor closeTypeIn.
	editor unselect.
	
	"Put the cursor after the `self` token, and then we will simulate code completion"
	self selectAt: 'self' size + 1.
	editor addString: ' te'.
	editor closeTypeIn.

	self expectText: 'self te
	b'.
	
	editor undo.
	self expectText: 'self
	b'.

	editor redo.
	self expectText: 'self te
	b'.
]

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> testRedoLeavesCursorInOriginalPosition [

	"If the caret is at the end of a word, replace the entire word"
	editor addString: 'self'.
	editor unselect.
	editor undo.
	editor redo.
	
	self expectText: 'self'.
	self assert: editor selectionInterval equals: (5 to: 4)
]

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> testUndoAfterTypeThenTabUndoesOnlyTheTab [

	editor addString: 'self'.
	editor unselect.
	editor tab: KeyboardEvent new.

	editor undo.

	self expectText: 'self'
]

{ #category : 'tests - undo' }
RubTextEditorLocalHistoryTest >> testUndoWordUndoesOneWordAtATime [

	editor addString: 'self'.
	editor unselect.
	editor space: KeyboardEvent new.
	
	editor addString: 'toto'.

	editor undo.

	self expectText: 'self'
]
