"
A column to display the row index of the collection being shown.
This is useful to add the index information (like happens in many spreadsheets, etc.)

The user does not needs to set an evaluation block for this column (as its value is taken elsewhere).

## Example
```
SpIndexTableColumn title: 'My index'
```
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
