"
I represent a tree column. I am used to specify how to render nodes, as well as the header title, icon , action on click and properties (resizable, initial width)
"
Class {
	#name : #TreeColumnPresenter,
	#superclass : #AbstractWidgetPresenter,
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
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
TreeColumnPresenter class >> adapterName [

	^ #TreeColumnAdapter
]

{ #category : #specs }
TreeColumnPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(TreeColumnAdapter
		adapt: #(model))
]

{ #category : #api }
TreeColumnPresenter >> container [

	^ container value
]

{ #category : #api }
TreeColumnPresenter >> container: aTreePresenter [

	container value: aTreePresenter
]

{ #category : #api }
TreeColumnPresenter >> displayBlock [

	^ displayBlock value
]

{ #category : #api }
TreeColumnPresenter >> displayBlock: aBlock [

	displayBlock value: aBlock
]

{ #category : #api }
TreeColumnPresenter >> headerAction [

	^ headerAction value
]

{ #category : #api }
TreeColumnPresenter >> headerAction: aBlock [

	headerAction value: aBlock
]

{ #category : #api }
TreeColumnPresenter >> headerFont [

	^ headerFont value
]

{ #category : #api }
TreeColumnPresenter >> headerFont: aFont [

	headerFont value: aFont
]

{ #category : #api }
TreeColumnPresenter >> headerIcon [

	^ headerIcon value
]

{ #category : #api }
TreeColumnPresenter >> headerIcon: anIcon [

	headerIcon value: anIcon
]

{ #category : #api }
TreeColumnPresenter >> headerLabel [

	^ headerLabel value
]

{ #category : #api }
TreeColumnPresenter >> headerLabel: aString [

	headerLabel value: aString
]

{ #category : #api }
TreeColumnPresenter >> initialWidth [

	^ initialWidth value
]

{ #category : #api }
TreeColumnPresenter >> initialWidth: anInteger [

	initialWidth value: anInteger
]

{ #category : #initialization }
TreeColumnPresenter >> initialize [

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
TreeColumnPresenter >> registerEvents [

	displayBlock whenChangedDo: [ self changed: #displayBlockChanged ].
	resizable whenChangedDo: [ self changed: #resizableChanged ].
	headerLabel whenChangedDo: [ self changed: #headerLabelChanged ].
	headerFont whenChangedDo: [ self changed: #headerFontChanged ].
	headerIcon whenChangedDo: [ self changed: #headerIconChanged ]
]

{ #category : #api }
TreeColumnPresenter >> resizable [

	^ resizable value
]

{ #category : #api }
TreeColumnPresenter >> resizable: aBoolean [

	resizable value: aBoolean
]
