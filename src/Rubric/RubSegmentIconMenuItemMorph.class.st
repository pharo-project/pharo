Class {
	#name : #RubSegmentIconMenuItemMorph,
	#superclass : #ToggleMenuItemMorph,
	#instVars : [
		'segment'
	],
	#category : 'Rubric-Editing-Widgets'
}

{ #category : #selecting }
RubSegmentIconMenuItemMorph >> deselect: anEvent [
	self segment showMouseHasLeaved.
	super deselect: anEvent.

]

{ #category : #events }
RubSegmentIconMenuItemMorph >> invokeWithEvent: evt [
	super invokeWithEvent: evt.
	self owner defaultTarget changed
]

{ #category : #accessing }
RubSegmentIconMenuItemMorph >> segment [
	
	^ segment
]

{ #category : #accessing }
RubSegmentIconMenuItemMorph >> segment: aSegment [
	
	segment := aSegment
]

{ #category : #selecting }
RubSegmentIconMenuItemMorph >> select: anEvent [
	self segment showMouseHasEntered.
	super select: anEvent.

]
