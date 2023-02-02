"
I just display an information box without altering the content of the text editor.

The UI should already have a lot of feedback (icons and highlighting), so there is no need to highjack the text content with superfluous information.
"
Class {
	#name : #RubInformStrategy,
	#superclass : #RubNotificationStrategy,
	#category : #'Rubric-Editing-Core'
}

{ #category : #notifications }
RubInformStrategy >> notify: aString at: anInteger in: aStream [
	"The compilation of text failed. Inform it to the user have a feedback that the method was not compiled/installed and place the cursor.
	
	Note: the original error node is lost, so it's that much we can do (yet)"

	| pos |
	self inform: aString.
	pos := self editor selectionInterval notEmpty
		       ifTrue: [ self editor startIndex + anInteger - 1 ]
		       ifFalse: [ anInteger ].
	self editor selectFrom: pos to: pos - 1. "move cursor in front of the location"
	self editor flash
]
