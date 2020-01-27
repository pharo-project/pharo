"
I extend a notebook by adding sortable capabilities.

"
Class {
	#name : #ClyNotebookMorph,
	#superclass : #SpNotebookMorph,
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #adding }
ClyNotebookMorph >> addPage: aMorph label: aStringOrMorph [
	"Add a page and its tab."
	| index |

	aMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	self pages add: aMorph.
	index := self pages indexOf: aMorph.
	self tabSelectorMorph tabs 
		add: (self tabSelectorMorph newLabelMorph: aStringOrMorph) 
		beforeIndex: index.
	self tabSelectorMorph updateTabs
]

{ #category : #adding }
ClyNotebookMorph >> useSortedTabsBy: aBlock [ 
	
	self pageMorphs: (SortedCollection sortBlock: aBlock)
]
