"
I'm a column to display the row index of the collection being shown.
This is useful to add the index information (like happens in many spreadsheets, etc.)

"
Class {
	#name : #SpIndexTableColumn,
	#superclass : #SpTableColumn,
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #visiting }
SpIndexTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitIndexColumn: self
]
