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
RubTextEditorLocalHistoryTest >> testUndoAfterFormatShouldRestoreOriginalCode [

	editor textArea beForSmalltalkCode.

	"Insert the start of the text and commit the transaction"
	editor addString: 'method (to be formatted; but) maybe not nice'.
	editor closeTypeIn.

	"let's replace the to be formatted by a shorter sequence.
	Do not commit the transaction here, simulating a text edition by a user.
	When the transaction is committed, two entries will get created:
	 - an entry with the removal of the selection
	 - an entry with the addition of the new text in that place"
	editor selectFrom: 'method (to ' size + 1 to: 'method (to be formatted' size.
	editor addString: 'format'.
	
	"Unselect and format. This should not break the undo buffer"
	editor unselect.
	editor textArea formatMethodCode.

	self expectText: 'method

	(to
		 format;
		 but) maybe not nice'.

	"Undo the format"	
	editor undo.
	self expectText: 'method (to format; but) maybe not nice'.	
	
	"Undo the addition of new text"
	editor undo.
	self expectText: 'method (to ; but) maybe not nice'.
	
	"Undo the removal"
	editor undo.
	self expectText: 'method (to be formatted; but) maybe not nice'
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
