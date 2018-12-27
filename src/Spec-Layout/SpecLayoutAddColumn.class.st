"
A SpecLayoutAddColumn is an action representing the addition of a column to a layout
"
Class {
	#name : #SpecLayoutAddColumn,
	#superclass : #SpecLayoutAdd,
	#category : #'Spec-Layout-Actions'
}

{ #category : #'instance creation' }
SpecLayoutAddColumn class >> block: aBlock layoutFrame: aLayoutFrame [

	^ self new
			block: aBlock 
			layoutFrame: aLayoutFrame;
			yourself
]

{ #category : #'instance creation' }
SpecLayoutAddColumn >> block: aBlock layoutFrame: aLayoutFrame [
	| column |
	column := SpecColumnLayout composed.
	aBlock value: column.
	subwidget := column.
	self layoutFrame: aLayoutFrame
]
