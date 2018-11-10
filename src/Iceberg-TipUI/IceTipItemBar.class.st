"
I'm a general bar to keep items inside. 
I can be extended to implement toolbars and statusbars.
"
Class {
	#name : #IceTipItemBar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'panel',
		'leftPanel',
		'rightPanel',
		'items',
		'color'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipItemBar >> addItem: aToolbarItem [
	items add: aToolbarItem.
	self refresh
]

{ #category : #'private dispatch' }
IceTipItemBar >> addItemLeft: aButton [
	self leftPanel addMorphBack: aButton
]

{ #category : #'private dispatch' }
IceTipItemBar >> addItemRight: aButton [
	self rightPanel addMorphBack: aButton
]

{ #category : #accessing }
IceTipItemBar >> color [
	^ color ifNil: [ Color transparent ]
]

{ #category : #accessing }
IceTipItemBar >> color: aColor [
	color := aColor
]

{ #category : #initialization }
IceTipItemBar >> initialize [
	super initialize.
	items := OrderedCollection new
]

{ #category : #initialization }
IceTipItemBar >> initializeWidgets [
	leftPanel := self newPanelMorph.
	rightPanel := self newPanelMorph
		listDirection: #rightToLeft;
		hResizing: #spaceFill.

	panel := PanelMorph new 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: self color;
		asSpecAdapter.
		
	panel widget 
		addMorphBack: leftPanel;
		addMorphBack: rightPanel

]

{ #category : #accessing }
IceTipItemBar >> items: aCollection [
	"We expect a collection to be made of specs"
	items := aCollection asOrderedCollection.
	self refresh
]

{ #category : #'private accessing' }
IceTipItemBar >> leftPanel [
	^ leftPanel
]

{ #category : #'private accessing' }
IceTipItemBar >> leftPanel: anObject [
	leftPanel := anObject
]

{ #category : #'private factory' }
IceTipItemBar >> newPanelMorph [
	^ PanelMorph new 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: self class defaultItemSeparation@0;
		color: self color;
		yourself
]

{ #category : #'accessing ui' }
IceTipItemBar >> panel [
	^ panel
]

{ #category : #'accessing ui' }
IceTipItemBar >> panel: anObject [
	panel := anObject
]

{ #category : #'private accessing' }
IceTipItemBar >> rightPanel [
	^ rightPanel
]

{ #category : #'private accessing' }
IceTipItemBar >> rightPanel: anObject [
	rightPanel := anObject
]
