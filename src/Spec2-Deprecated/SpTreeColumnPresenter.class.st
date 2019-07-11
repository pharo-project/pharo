"
I represent a tree column. I am used to specify how to render nodes, as well as the header title, icon , action on click and properties (resizable, initial width)
"
Class {
	#name : #SpTreeColumnPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'container',
		'displayBlock',
		'initialWidth',
		'resizable',
		'headerLabel',
		'headerFont',
		'headerIcon',
		'headerAction'
	],
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpTreeColumnPresenter class >> adapterName [

	^ #TreeColumnAdapter
]

{ #category : #specs }
SpTreeColumnPresenter class >> isDeprecated [ 

	^ true
]

{ #category : #api }
SpTreeColumnPresenter >> container [

	^ container value
]

{ #category : #api }
SpTreeColumnPresenter >> container: aTreePresenter [

	container value: aTreePresenter
]

{ #category : #api }
SpTreeColumnPresenter >> displayBlock [

	^ displayBlock value
]

{ #category : #api }
SpTreeColumnPresenter >> displayBlock: aBlock [

	displayBlock value: aBlock
]

{ #category : #api }
SpTreeColumnPresenter >> headerAction [

	^ headerAction value
]

{ #category : #api }
SpTreeColumnPresenter >> headerAction: aBlock [

	headerAction value: aBlock
]

{ #category : #api }
SpTreeColumnPresenter >> headerFont [

	^ headerFont value
]

{ #category : #api }
SpTreeColumnPresenter >> headerFont: aFont [

	headerFont value: aFont
]

{ #category : #api }
SpTreeColumnPresenter >> headerIcon [

	^ headerIcon value
]

{ #category : #api }
SpTreeColumnPresenter >> headerIcon: anIcon [

	headerIcon value: anIcon
]

{ #category : #api }
SpTreeColumnPresenter >> headerLabel [

	^ headerLabel value
]

{ #category : #api }
SpTreeColumnPresenter >> headerLabel: aString [

	headerLabel value: aString
]

{ #category : #api }
SpTreeColumnPresenter >> initialWidth [

	^ initialWidth value
]

{ #category : #api }
SpTreeColumnPresenter >> initialWidth: anInteger [

	initialWidth value: anInteger
]

{ #category : #initialization }
SpTreeColumnPresenter >> initialize [

	super initialize.

	container := nil asValueHolder.
	displayBlock := [ :node | node printString ] asValueHolder.
	initialWidth := 100 asValueHolder.
	resizable := true asValueHolder.
	headerLabel := nil asValueHolder.
	headerFont := self class defaultFont asValueHolder.
	headerIcon := nil asValueHolder.
	headerAction := nil asValueHolder.
	
	self registerEvents
]

{ #category : #initialization }
SpTreeColumnPresenter >> registerEvents [

	displayBlock whenChangedDo: [ self changed: #displayBlockChanged ].
	resizable whenChangedDo: [ self changed: #resizableChanged ].
	headerLabel whenChangedDo: [ self changed: #headerLabelChanged ].
	headerFont whenChangedDo: [ self changed: #headerFontChanged ].
	headerIcon whenChangedDo: [ self changed: #headerIconChanged ]
]

{ #category : #api }
SpTreeColumnPresenter >> resizable [

	^ resizable value
]

{ #category : #api }
SpTreeColumnPresenter >> resizable: aBoolean [

	resizable value: aBoolean
]
