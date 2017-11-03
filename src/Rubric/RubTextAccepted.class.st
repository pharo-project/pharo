"
I am raised when the text has been accepted
"
Class {
	#name : #RubTextAccepted,
	#superclass : #RubMorphAnnouncement,
	#category : #Rubric-Editing-Announcements
}

{ #category : #accessing }
RubTextAccepted >> text [ 
	^ morph textArea text
]
