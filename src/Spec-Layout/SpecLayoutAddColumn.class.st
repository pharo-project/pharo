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

	| row |
	row := SpecColumnLayout composed.
	aBlock value: row.
	subwidget := row asArray.
	self layoutFrame: aLayoutFrame.
	self generateArguments
]
