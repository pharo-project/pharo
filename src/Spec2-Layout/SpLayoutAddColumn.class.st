"
A SpecLayoutAddColumn is an action representing the addition of a column to a layout
"
Class {
	#name : #SpLayoutAddColumn,
	#superclass : #SpLayoutAdd,
	#category : #'Spec2-Layout-Deprecated'
}

{ #category : #'instance creation' }
SpLayoutAddColumn class >> block: aBlock layoutFrame: aLayoutFrame [

	^ self new
			block: aBlock 
			layoutFrame: aLayoutFrame;
			yourself
]

{ #category : #'instance creation' }
SpLayoutAddColumn >> block: aBlock layoutFrame: aLayoutFrame [
	| column |
	column := SpColumnLayout composed.
	aBlock value: column.
	subwidget := column.
	self layoutFrame: aLayoutFrame
]
