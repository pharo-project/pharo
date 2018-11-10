"
I place item buttons at left. 
I am the default behaviour.
"
Class {
	#name : #IceTipItemPositionLeft,
	#superclass : #IceTipItemPosition,
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipItemPositionLeft >> addItem: anObject into: aToolbar [
	aToolbar addItemLeft: anObject
]
