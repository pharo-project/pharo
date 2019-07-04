Class {
	#name : #SpNotebookTabSelectorMorph,
	#superclass : #TabSelectorMorph,
	#category : #'Spec2-Adapters-Morphic-Notebook'
}

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
