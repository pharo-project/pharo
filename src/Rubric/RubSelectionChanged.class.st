"
I am raised when the current selection is modified 
"
Class {
	#name : #RubSelectionChanged,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'previousMarkBlock',
		'previousPointBlock'
	],
	#category : #'Rubric-Editing-Announcements'
}

{ #category : #'instance creation' }
RubSelectionChanged class >> from: aTextArea previousMarkBlock: prevMarkBlock previousPointBlock: prevPointBlock [ 
	^ self new from: aTextArea previousMarkBlock: prevMarkBlock previousPointBlock: prevPointBlock 

]

{ #category : #accessing }
RubSelectionChanged >> from: aTextArea previousMarkBlock: prevMarkBlock previousPointBlock: prevPointBlock [ 
	self morph: aTextArea.
	previousMarkBlock := prevMarkBlock.
	previousPointBlock := prevPointBlock
]

{ #category : #accessing }
RubSelectionChanged >> markBlock [
	^ self textArea markBlock
]

{ #category : #accessing }
RubSelectionChanged >> pointBlock [
	^ self textArea pointBlock
]

{ #category : #accessing }
RubSelectionChanged >> previousMarkBlock [
	^ previousMarkBlock
]

{ #category : #accessing }
RubSelectionChanged >> previousPointBlock [
	^ previousPointBlock
]

{ #category : #accessing }
RubSelectionChanged >> textArea [
	^ morph
]
