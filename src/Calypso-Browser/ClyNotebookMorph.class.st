"
I extend a notebook by adding sortable capabilities.

"
Class {
	#name : #ClyNotebookMorph,
	#superclass : #SpNotebookMorph,
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #accessing }
ClyNotebookMorph >> addActions: aCollection [

	self toolbarMorph 
		removeAllMorphs;
		addAllMorphs: aCollection
]

{ #category : #accessing }
ClyNotebookMorph >> addPage: aMorph label: aStringOrMorph [
	"Add a page and its tab."
	| index |
	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self pages add: aMorph.
	aMorph privateOwner: self.
	
	index := self pages indexOf: aMorph.
	self tabSelectorMorph addTab: aStringOrMorph beforeIndex: index
]

{ #category : #'adding-removing' }
ClyNotebookMorph >> addToSelection: aPage [ 

	self selectedPageIndex: (pageMorphs indexOf: aPage)
]

{ #category : #initialization }
ClyNotebookMorph >> initialize [

	toolbarMorph := self newToolbarMorph.
	super initialize
]

{ #category : #private }
ClyNotebookMorph >> newHeaderMorph [

	^ (self theme newRowIn: self theme for: { self tabSelectorMorph. self toolbarMorph }) 
		cellInset: 0; 
		cellPositioning: #bottomCenter; 
		borderWidth: 0
]

{ #category : #factory }
ClyNotebookMorph >> newTabSelectorMorph [

	^ super newTabSelectorMorph
		color: self theme baseColor;
		yourself
]

{ #category : #private }
ClyNotebookMorph >> newToolbarMorph [

	^ SpNotebookPanelMorph new
		fillStyle: (SolidFillStyle color: self theme baseColor);
		borderWidth: 2;
		borderColor: self theme baseColor;
		vResizing: #spaceFill;
		hResizing: #shrinkWrap;
		cellPositioning: #center;
		changeTableLayout;
		listDirection:#leftToRight;
		cellInset: 4;
		yourself 
]

{ #category : #private }
ClyNotebookMorph >> postAddPage: aPage [

	self addActions: aPage actions
]

{ #category : #'adding-removing' }
ClyNotebookMorph >> removeFromSelection: aPage [ 

	self selectedPageIndex = (pageMorphs indexOf: aPage)
		ifTrue: [ self selectedPageIndex: 0 ].
]

{ #category : #theme }
ClyNotebookMorph >> themeChanged [

	super themeChanged.
	self toolbarMorph 
		fillStyle: (SolidFillStyle color: self theme baseColor);
		borderColor: self theme baseColor.
	self tabSelectorMorph
		color: self theme baseColor.
	self pages do: [ :each | each themeChanged ]
]

{ #category : #accessing }
ClyNotebookMorph >> toolbarMorph [

	^ toolbarMorph
]

{ #category : #accessing }
ClyNotebookMorph >> useSortedTabsBy: aBlock [ 
	
	self pageMorphs: (SortedCollection sortBlock: aBlock)
]
