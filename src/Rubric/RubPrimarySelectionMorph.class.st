"
I represent the morph which shows the current selection 
"
Class {
	#name : #RubPrimarySelectionMorph,
	#superclass : #RubPlainSegmentMorph,
	#category : #Rubric-Editing-Core
}

{ #category : #initialization }
RubPrimarySelectionMorph >> defaultBorderColor [
	^ Color orange

]

{ #category : #initialization }
RubPrimarySelectionMorph >> defaultColor [
	^ self theme selectionColor

]

{ #category : #initialization }
RubPrimarySelectionMorph >> initialize [
	super initialize.
	self beExtendable
]

{ #category : #ui }
RubPrimarySelectionMorph >> readWriteView [
	| view |
	view  := super readWriteView.
	^ view
]

{ #category : #'event handling' }
RubPrimarySelectionMorph >> registerTextArea [ 
	super registerTextArea.
	
	textArea announcer 
	when:  RubSelectionChanged send: #whenTextAreaSelectionChanged: to: self;
	when:  MorphGotFocus send: #whenTextAreaGotFocus: to: self;	
	when:  MorphLostFocus send: #whenTextAreaLostFocus: to: self.	

]

{ #category : #accessing }
RubPrimarySelectionMorph >> selectionColor [
	^ self textArea hasFocus
		ifTrue: [ self theme selectionColor ]
		ifFalse: [ self theme lessConspicuousColorFrom: (self theme unfocusedSelectionColor) ]
]

{ #category : #protocol-events }
RubPrimarySelectionMorph >> whenTextAreaGotFocus: anAnnouncement [ 
	self color: self selectionColor 
]

{ #category : #protocol-events }
RubPrimarySelectionMorph >> whenTextAreaLostFocus: anAnnouncement [ 
	self color: self selectionColor 
]

{ #category : #protocol-events }
RubPrimarySelectionMorph >> whenTextAreaSelectionChanged: anAnnouncement [ 
	firstIndex := self textArea markIndex min: self textArea pointIndex.
	lastIndex := self textArea pointIndex max: self textArea markIndex.
	self computeVertices.
	self announcer announce: RubTextSetInModel.

]
