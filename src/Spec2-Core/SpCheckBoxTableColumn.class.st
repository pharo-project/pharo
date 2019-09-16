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
SpCheckBoxTableColumn >> onActivation: anObject [
	onActivation := anObject
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDesactivation [
	^ onDesactivation
]

{ #category : #accessing }
SpCheckBoxTableColumn >> onDesactivation: anObject [
	onDesactivation := anObject
]
