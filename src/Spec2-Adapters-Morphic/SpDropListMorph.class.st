"
I am a special DropList with icons
"
Class {
	#name : #SpDropListMorph,
	#superclass : #DropListMorph,
	#instVars : [
		'currentIcon'
	],
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #configuring }
SpDropListMorph >> configureWith: displayModel item: itemPresenter [ 
	displayModel configureDropList: self item: itemPresenter
]

{ #category : #private }
SpDropListMorph >> currentIcon [

	^ currentIcon
]

{ #category : #protocol }
SpDropListMorph >> getIconFor: anItem [

	^ self model getIconFor: anItem
]

{ #category : #private }
SpDropListMorph >> listHeight [
	"Answer the height for the list."

	^ super listHeight + 12
]

{ #category : #private }
SpDropListMorph >> listMorphClass [

	^ PluggableIconListMorph
]

{ #category : #drawing }
SpDropListMorph >> newListMorph [
	"Answer a new list morph"

	^ (self listMorphClass
		on: self
		list: #list
		selected: #listSelectionIndex
		changeSelected: #listSelectionIndex:
		menu: nil
		keystroke: nil)
			getIconSelector: #getIconFor:;
			autoDeselect: false;
			wrapSelector: #wrapItem:index:;
			roundedCorners: #(2 3);
			setProperty: #morphicLayerNumber toValue: 5;
			borderStyle: (self theme dropListNormalListBorderStyleFor: self);
			on: #mouseDown send: #listMouseDown: to: self;
			yourself
]

{ #category : #private }
SpDropListMorph >> updateListSelectionIndex [

	currentIcon ifNotNil: [ self removeMorph: currentIcon ].
		
	super updateListSelectionIndex.
	
	self selectedItem ifNotNil: [ :item | 
		(self getIconFor: item) ifNotNil: [:icon | 
			currentIcon := icon asMorph.
			self addMorphFront: currentIcon ] ] 
]

{ #category : #private }
SpDropListMorph >> wrapItem: anItem index: anIndex [

	^ self model wrapItem: anItem index: anIndex
]
