Class {
	#name : #SpMorphicPopoverAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicPopoverAdapter class >> defaultHeight [

	^ 30
]

{ #category : #accessing }
SpMorphicPopoverAdapter class >> defaultItemSeparation [
	
	^ 3@0
]

{ #category : #factory }
SpMorphicPopoverAdapter >> addModelTo: aMorph [

	aMorph contentMorph: self presenterWidget.	
	aMorph beRelativeTo: self presenter relativeTo adapter widget. 	
	self setPopoverPositionTo: aMorph.
	
	self presenter whenPresenterChangedDo: [
		aMorph contentMorph: self presenterWidget ]
]

{ #category : #factory }
SpMorphicPopoverAdapter >> buildWidget [
	| panelMorph |
	
	panelMorph := SpPopoverMorph new.
	self addModelTo: panelMorph.
	
	^ panelMorph
]

{ #category : #api }
SpMorphicPopoverAdapter >> dismiss [ 

	self widgetDo: [ :w | w popdown ]
]

{ #category : #api }
SpMorphicPopoverAdapter >> popup [

	self widgetDo: [ :w | w popup ]
]

{ #category : #api }
SpMorphicPopoverAdapter >> popupPointingTo: aRectangle [

	self widgetDo: [ :w | 
		w popupPointingTo: aRectangle ]
]

{ #category : #factory }
SpMorphicPopoverAdapter >> presenterWidget [

	^ self presenter presenter buildWithSpec
]

{ #category : #factory }
SpMorphicPopoverAdapter >> setPopoverPositionTo: gtkPopover [
		
	^ self presenter position applyTo: gtkPopover
]
