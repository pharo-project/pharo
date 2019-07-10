"
I'm a column to display images.
"
Class {
	#name : #SpImageTableColumn,
	#superclass : #SpTableColumn,
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #visiting }
SpImageTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitImageColumn: self
]
