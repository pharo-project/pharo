"
A RubHighlightSegmentMorph is a volatile segment that deletes itself whenever the user interacts with the editor. It can be used to add selection-like segments.

"
Class {
	#name : #RubHighlightSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#category : 'Rubric-Editing-Core'
}

{ #category : #'event handling' }
RubHighlightSegmentMorph >> handlesMouseDown: evt [

	^ true
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> mouseDown: anEvent [
	super mouseDown: anEvent.
	self delete
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> registerTextArea [
	super registerTextArea.
	
	textArea announcer  
		when: RubKeystroke send: #whenKeyPressed: to: self;
		when: RubMouseClick send: #whenMouseClicked: to: self;
		when: RubSelectionChanged send: #whenSelectionChanged: to: self.
	
	self color: (self theme rubScrolledTextMorphThemer highlightBackgroundColorFor: textArea).
	self borderStyle: (self theme rubScrolledTextMorphThemer highlightBorderStyleFor: textArea).
	self borderWidth: (self textArea hasSelection
		ifTrue: [ 2 * self borderWidth] 
		ifFalse: [ self borderWidth ])
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> showMouseHasEntered [

]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> showMouseHasLeaved [


]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> whenKeyPressed: anEvent [

	self delete
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> whenMouseClicked: event [

	self delete
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> whenSelectionChanged: anAnnouncement [ 

	self delete
	
]

{ #category : #'event handling' }
RubHighlightSegmentMorph >> whenTextChanged: anAnnouncement [ 

	self delete
]
