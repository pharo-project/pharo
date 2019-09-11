"
I insert the notification directly in the text at the given position. (I alter the content of the text editor)
"
Class {
	#name : #RubTextInsertionStrategy,
	#superclass : #RubNotificationStrategy,
	#category : #'Rubric-Editing-Core'
}

{ #category : #notification }
RubTextInsertionStrategy >> notify: aString at: anInteger in: aStream [ 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."

	| pos |
	pos := self editor selectionInterval notEmpty
		ifTrue: [self editor startIndex + anInteger - 1 ]
		ifFalse: [anInteger].
	self editor insertAndSelect: aString at: (pos max: 1)
]
