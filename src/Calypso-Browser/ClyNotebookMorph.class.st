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
	self tabSelectorMorph tabs 
		add: (self tabSelectorMorph newLabelMorph: aStringOrMorph) 
		beforeIndex: index.
	self tabSelectorMorph updateTabs
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

{ #category : #private }
ClyNotebookMorph >> newToolbarMorph [

	^ SpNotebookPanelMorph new
		fillStyle: (SolidFillStyle color: Color transparent);
		borderWidth: 2;
		borderColor: Color transparent;
		vResizing: #spaceFill;
		hResizing: #shrinkWrap;
		cellPositioning: #center;
		changeTableLayout;
		listDirection:#leftToRight;
		cellInset: 3;
		yourself 
]

{ #category : #private }
ClyNotebookMorph >> postAddPage: aPage [

	self addActions: aPage actions
]

{ #category : #accessing }
ClyNotebookMorph >> toolbarMorph [

	^ toolbarMorph
]

{ #category : #accessing }
ClyNotebookMorph >> useSortedTabsBy: aBlock [ 
	
	self pageMorphs: (SortedCollection sortBlock: aBlock)
]
