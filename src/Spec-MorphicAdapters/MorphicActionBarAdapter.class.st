Class {
	#name : #MorphicActionBarAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #accessing }
MorphicActionBarAdapter class >> defaultHeight [

	^ 30
]

{ #category : #accessing }
MorphicActionBarAdapter class >> defaultItemSeparation [
	
	^ 3@0
]

{ #category : #factory }
MorphicActionBarAdapter >> addModelTo: aMorph [

	self flag: #TODO. "Maybe validate they are just buttons inside?"
	(self model items at: #start ifAbsent: [ #() ]) do: [ :each |
		aMorph submorphs first addMorphBack: (self configureActionButton: each buildWithSpec) ].
	(self model items at: #end ifAbsent: [ #() ]) reverseDo: [ :each |
		aMorph submorphs second addMorphBack: (self configureActionButton: each buildWithSpec) ]
]

{ #category : #factory }
MorphicActionBarAdapter >> buildWidget [
	| panelMorph |
	
	"Using a toolbar morph because is more or less the same, it just change 
	 the kind of elements it can have inside (which are just buttons)"
	panelMorph := ToolBarMorph new.
	self addModelTo: panelMorph.
	
	^ panelMorph
]

{ #category : #factory }
MorphicActionBarAdapter >> configureActionButton: aMorph [	
		
	(self styleAt: #ActionButton) applyTo: aMorph.
	^ aMorph
]

{ #category : #factory }
MorphicActionBarAdapter >> newPanelMorph [
	
	^ Morph new 
		changeTableLayout;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: self class defaultHeight;
		cellInset: self class defaultItemSeparation;
		color: Color random;
		yourself
]
