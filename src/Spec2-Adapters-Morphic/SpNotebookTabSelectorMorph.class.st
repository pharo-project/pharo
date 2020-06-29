Class {
	#name : #SpNotebookTabSelectorMorph,
	#superclass : #TabSelectorMorph,
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

{ #category : #operations }
SpNotebookTabSelectorMorph >> newLabelMorph: aStringOrMorph [
	"Answer a new label morph with the given label text."

	^ TabLabelMorph new
		roundedCorners: #(1 4);
		cornerStyle: self cornerStyle;
		changeTableLayout;
		listDirection: #leftToRight;
		listCentering: #center;
		layoutInset: (self theme tabLabelInsetFor: self);
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		label: aStringOrMorph;
		in: [ :this | self font ifNotNil: [ this font: self font ] ];
		on: #mouseDown send: #tabClicked:with: to: self;
		tabSelector: self
]

{ #category : #operations }
SpNotebookTabSelectorMorph >> newLabelMorph: aStringOrMorph selected: selectedStringOrMorph [
	"Answer a new label morph with the given label text or morph and
	alternate when selected."

	^ TabLabelMorph new
		roundedCorners: #(1 4);
		cornerStyle: self cornerStyle;
		changeTableLayout;
		listDirection: #leftToRight;
		listCentering: #center;
		layoutInset: (self theme tabLabelInsetFor: self);
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		label: aStringOrMorph selected: selectedStringOrMorph;
		in: [ :this | self font ifNotNil: [ this font: self font ] ];
		on: #mouseDown send: #tabClicked:with: to: self;
		tabSelector: self
]

{ #category : #operations }
SpNotebookTabSelectorMorph >> removeTabIndex: anInteger [
	"Remove the tab at the given index and
	adjust any selected (next or first if was last)."

	self tabs removeAt: anInteger.

	self tabs
		ifNotEmpty: [ self selectedIndex: (self selectedIndex - 1) \\ self tabs size + 1 ]
		ifEmpty: [ self selectedIndex: 0 ].

	self updateTabs
]
