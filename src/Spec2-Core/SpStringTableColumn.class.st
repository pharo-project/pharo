"
A column to display strings.
A `SpStringTableColumn` cell can be edited (see `SpStringTableColumn>>#beEditable` and `SpStringTableColumn>>#onAcceptEdition:`)

The user need to set the evaluation block (by sending `SpTableColumn>>#evaluated:`) to answer an image (an instance of `Form`).

## Example
```
SpStringTableColumn 
	title: 'My string' 
	evaluated: [ :anObject | anObject printString ]
```
"
Class {
	#name : #SpStringTableColumn,
	#superclass : #SpTableColumn,
	#instVars : [
		'editable',
		'acceptAction',
		'sortable'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #api }
SpStringTableColumn >> acceptAction [
	"Answer the action (set by `SpStringTablePresenter>>#onAcceptEdition:`) to execute when accepting an edition."
	
	^ acceptAction
]

{ #category : #visiting }
SpStringTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitStringColumn: self
]

{ #category : #api }
SpStringTableColumn >> beEditable [ 
	"Set the cell as editable."

	editable := true
]

{ #category : #api }
SpStringTableColumn >> beNotEditable [ 
	"Set the cell as not editable (this is the default)."

	editable := false
]

{ #category : #api }
SpStringTableColumn >> beNotSortable [
	"Set the colum as not sortable."

	sortable := false
]

{ #category : #api }
SpStringTableColumn >> beSortable [
	"Set the cell as sortable (this is the default)"

	sortable := true
]

{ #category : #initialization }
SpStringTableColumn >> initialize [ 

	super initialize.
	editable := false.
	sortable := true
]

{ #category : #testing }
SpStringTableColumn >> isEditable [ 
	"Answer true if column has editable cells"
	
	^ editable
]

{ #category : #testing }
SpStringTableColumn >> isSortable [
	"Answer true if column is sortable"

	^ sortable
]

{ #category : #private }
SpStringTableColumn >> isSortable: aBoolean [ 
	sortable := aBoolean
]

{ #category : #api }
SpStringTableColumn >> onAcceptEdition: aBlock [
	"Set the block to execute when cell edition is accepted.
	 `aBlock` receives two arguments: 
		- the element of the table (See `SpAbstractListPresenter>>#items:`
		- the string entered while editing"

	acceptAction := aBlock
]
