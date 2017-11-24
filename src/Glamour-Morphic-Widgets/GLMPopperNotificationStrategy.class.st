"
I opened a new popper window that displays the given notification. (I do not alter the content of the text editor.)
"
Class {
	#name : #GLMPopperNotificationStrategy,
	#superclass : #RubNotificationStrategy,
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #notification }
GLMPopperNotificationStrategy >> notify: aString at: anInteger in: aStream [
	| pos |
	pos := self editor selectionInterval notEmpty
		ifTrue: [self editor startIndex + anInteger - 1 ]
		ifFalse: [anInteger].
	self editor selectFrom: pos to: pos.
	
	GLMErrorPopper
		installAlarm: #popupError:
		for: self
		withArguments: { aString }
]

{ #category : #callbacks }
GLMPopperNotificationStrategy >> popupError: aString [
	(GLMErrorPopper new withString: aString from: self editor textArea owner owner owner)
		openInWorld
]
