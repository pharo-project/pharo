"
I'm a column to display images.
"
Class {
	#name : #ImageTableColumn,
	#superclass : #TableColumn,
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #visiting }
ImageTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitImageColumn: self
]
