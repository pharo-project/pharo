Class {
	#name : #SpCheckBoxTableColumn,
	#superclass : #SpTableColumn,
	#instVars : [
		'onActivation',
		'onDesactivation'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #visiting }
SpCheckBoxTableColumn >> acceptColumnVisitor: aBuilder [

	^ aBuilder visitCheckboxColumn: self
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onActivation [

	^ onActivation
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onActivation: aBlock [

	onActivation := aBlock
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDeactivation [

	^ onDesactivation
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDeactivation: aBlock [

	onDesactivation := aBlock
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDesactivation [

	^ onDesactivation
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDesactivation: aBlock [

	onDesactivation := aBlock
]
