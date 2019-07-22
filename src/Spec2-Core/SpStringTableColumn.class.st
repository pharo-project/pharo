"
I'm a column to display strings.
A StringTableColumn cell can be edited (see #beEditable and onAcceptEdition:)

TablePresenter exampleEditableCell.
"
Class {
	#name : #SpStringTableColumn,
	#superclass : #SpTableColumn,
	#instVars : [
		'editable',
		'acceptAction'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #accessing }
SpStringTableColumn >> acceptAction [

	^ acceptAction
]

{ #category : #visiting }
SpStringTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitStringColumn: self
]

{ #category : #accessing }
SpStringTableColumn >> beEditable [ 

	editable := true
]

{ #category : #initialization }
SpStringTableColumn >> initialize [ 

	super initialize.
	editable := false
]

{ #category : #testing }
SpStringTableColumn >> isEditable [ 
	
	^ editable
]

{ #category : #accessing }
SpStringTableColumn >> onAcceptEdition: aBlock [

	acceptAction := aBlock
]
