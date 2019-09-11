Class {
	#name : #SpMorphicActionBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicActionBarAdapter class >> defaultHeight [

	^ 30
]

{ #category : #accessing }
SpMorphicActionBarAdapter class >> defaultItemSeparation [
	
	^ 3@0
]

{ #category : #factory }
SpMorphicActionBarAdapter >> addModelTo: aMorph [

	self flag: #TODO. "Maybe validate they are just buttons inside?"
	(self model items at: #start ifAbsent: [ #() ]) do: [ :each |
		aMorph submorphs first addMorphBack: (self configureActionButton: each buildWithSpec) ].
	(self model items at: #end ifAbsent: [ #() ]) reverseDo: [ :each |
		aMorph submorphs second addMorphBack: (self configureActionButton: each buildWithSpec) ]
]

{ #category : #factory }
SpMorphicActionBarAdapter >> buildWidget [
	| panelMorph |
	
	"Using a toolbar morph because is more or less the same, it just change 
	 the kind of elements it can have inside (which are just buttons)"
	panelMorph := SpToolBarMorph new.
	self addModelTo: panelMorph.
	
	^ panelMorph
]

{ #category : #factory }
SpMorphicActionBarAdapter >> configureActionButton: aMorph [	
		
	(self styleAt: #ActionButton) applyTo: aMorph.
	^ aMorph
]

{ #category : #factory }
SpMorphicActionBarAdapter >> newPanelMorph [
	
	^ Morph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: self class defaultHeight;
		cellInset: self class defaultItemSeparation;
		color: Color random;
		yourself
]
