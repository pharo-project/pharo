Class {
	#name : #StDebuggerTreeTablePresenter,
	#superclass : #SpTreeTablePresenter,
	#instVars : [
		'items',
		'filter',
		'counter'
	],
	#category : #'NewTools-Debugger-View'
}

{ #category : #api }
StDebuggerTreeTablePresenter >> applyFilter [

	filter ifNil: [ ^items ].
	self flag: 'Ugly hack because links presenter trigger 2 times their action. But for now it works'.
	counter > 3 ifTrue:  [ counter := 0. filter := nil. ^items ].

	^items select: [:item| item variableTag = filter]
]

{ #category : #accessing }
StDebuggerTreeTablePresenter >> filter [
	^ filter
]

{ #category : #api }
StDebuggerTreeTablePresenter >> hasActiveFilter [
	^counter > 0
]

{ #category : #initialization }
StDebuggerTreeTablePresenter >> initialize [
	super initialize.
	counter := 0
]

{ #category : #api }
StDebuggerTreeTablePresenter >> roots: aCollection [

	items := aCollection.
	super roots: self applyFilter

]

{ #category : #api }
StDebuggerTreeTablePresenter >> toggleFilter: aVariableTag [

"	filter := filter = aVariableTag
		          ifTrue: [ nil ]
		          ifFalse: [ aVariableTag ]."
	filter := aVariableTag.
	counter := counter + 1.

	self roots: items
]
