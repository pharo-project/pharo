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
	"The compilation of text failed. Infor it to the user have a feedback that the method was not compiled/installed."

	self inform: aString
]
