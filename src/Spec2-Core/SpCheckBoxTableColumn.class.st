Class {
	#name : #SpCheckBoxTableColumn,
	#superclass : #SpTableColumn,
	#instVars : [
		'onActivation',
		'onDeactivation'
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

	^ onDeactivation
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDeactivation: aBlock [

	onDeactivation := aBlock
]
