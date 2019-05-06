Class {
	#name : #CheckBoxTableColumn,
	#superclass : #TableColumn,
	#instVars : [
		'onActivation',
		'onDesactivation'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #visiting }
CheckBoxTableColumn >> acceptColumnVisitor: aBuilder [
	^ aBuilder visitCheckboxColumn: self
]

{ #category : #accessing }
CheckBoxTableColumn >> onActivation [
	^ onActivation
]

{ #category : #accessing }
CheckBoxTableColumn >> onActivation: anObject [
	onActivation := anObject
]

{ #category : #accessing }
CheckBoxTableColumn >> onDesactivation [
	^ onDesactivation
]

{ #category : #accessing }
CheckBoxTableColumn >> onDesactivation: anObject [
	onDesactivation := anObject
]

{ #category : #accessing }
CheckBoxTableColumn >> readObject: anItem [
	| presenter |
	presenter := CheckBoxPresenter new
		state: (super readObject: anItem);
		yourself.

	self onActivation ifNotNil: [ :valuable | presenter whenActivatedDo: [ valuable cull: anItem ] ].
	self onDesactivation ifNotNil: [ :valuable | presenter whenDeactivatedDo: [ valuable cull: anItem ] ].

	^ presenter buildWithSpec
]
