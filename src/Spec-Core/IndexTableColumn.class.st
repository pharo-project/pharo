"
I'm a column to display the row index of the collection being shown.
This is useful to add the index information (like happens in many spreadsheets, etc.)

"
Class {
	#name : #IndexTableColumn,
	#superclass : #TableColumn,
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #visiting }
IndexTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitIndexColumn: self
]
