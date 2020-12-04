"
A column that displays images.

The user need to set the evaluation block (by sending `SpTableColumn>>#evaluated:`) to answer an image (an instance of `Form`).

## Example
```
SpImageTableColumn 
	title: 'My Image' 
	evaluated: [ :anObject | anObject systemIcon ]
```
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
