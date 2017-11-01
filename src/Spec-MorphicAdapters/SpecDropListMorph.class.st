"
I am a special DropList with icons
"
Class {
	#name : #SpecDropListMorph,
	#superclass : #DropListMorph,
	#instVars : [
		'currentIcon'
	],
	#category : 'Spec-MorphicAdapters-Support'
}

{ #category : #protocol }
SpecDropListMorph >> getIconFor: anItem [

	^ self model getIconFor: anItem
]

{ #category : #private }
SpecDropListMorph >> listHeight [
	"Answer the height for the list."

	^ super listHeight + 12
]

{ #category : #private }
SpecDropListMorph >> listMorphClass [

	^ PluggableIconListMorph
]

{ #category : #drawing }
SpecDropListMorph >> newListMorph [
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
SpecDropListMorph >> updateListSelectionIndex [

	currentIcon ifNotNil: [ self removeMorph: currentIcon ].
		
	super updateListSelectionIndex.
	
	self selectedItem ifNotNil: [ :item | 
		(self getIconFor: item) ifNotNil: [:icon | 
			currentIcon := icon asMorph.
			self addMorphFront: currentIcon ] ] 
]

{ #category : #private }
SpecDropListMorph >> wrapItem: anItem index: anIndex [

	^ self model wrapItem: anItem index: anIndex
]
