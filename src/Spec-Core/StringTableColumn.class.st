"
I'm a column to display strings.
A StringTableColumn cell can be edited (see #beEditable and onAcceptEdition:)

TablePresenter exampleEditableCell.
"
Class {
	#name : #StringTableColumn,
	#superclass : #TableColumn,
	#instVars : [
		'editable',
		'acceptAction'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #accessing }
StringTableColumn >> acceptAction [

	^ acceptAction
]

{ #category : #visiting }
StringTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitStringColumn: self
]

{ #category : #accessing }
StringTableColumn >> beEditable [ 

	editable := true
]

{ #category : #initialization }
StringTableColumn >> initialize [ 

	super initialize.
	editable := false
]

{ #category : #testing }
StringTableColumn >> isEditable [ 
	
	^ editable
]

{ #category : #accessing }
StringTableColumn >> onAcceptEdition: aBlock [

	acceptAction := aBlock
]
