"
This morph can be used to show a list having multiple columns,  The columns are self width sized to make the largest entry in each list fit.  In some cases the pane may then be too narrow.

Use it like a regular PluggableListMorph except pass in an array of lists instead of a single list.

There are base assumptions made here that each list in the array of lists is the same size.

Also, the highlight color for the selection is easy to modify in the #highlightSelection method.  I used blue
when testing just to see it work.
"
Class {
	#name : #PluggableMultiColumnListMorph,
	#superclass : #PluggableListMorph,
	#instVars : [
		'gapSize'
	],
	#category : #Morphic-Widgets-Pluggable
}

{ #category : #'instance creation' }
PluggableMultiColumnListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel wrapSelector: wrapSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel
		wrapSelector: wrapSel
]

{ #category : #'instance creation' }
PluggableMultiColumnListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel wrapSelector: wrapSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: #arrowKey:from:	"default"
		wrapSelector: wrapSel
]

{ #category : #'model access' }
PluggableMultiColumnListMorph >> basicKeyPressed: aChar [
	"net supported for multi-column lists; which column should be used?!  The issue is that the base class implementation uses getList expecting a single collectino to come back instead of several of them"
	^self
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> findSubmorphFor: ptY [
	^ scroller submorphThat: [ :item | ptY between: item top and: item bottom ] ifNone: [  ]
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> gapSize [

	^ gapSize ifNil: [ gapSize := 10 ]
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> gapSize: anInteger [

	gapSize := anInteger.
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> getListRow: row [
	"return the strings that should appear in the requested row"
	getListElementSelector ifNotNil: [ ^model perform: getListElementSelector with: row ].
	^self getList at: row
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> getListSize [
	| l |
	getListSizeSelector ifNotNil: [ ^model perform: getListSizeSelector ].

	l := self getList.
	l isEmpty ifTrue: [ ^ 0 ].
	^l size
]

{ #category : #searching }
PluggableMultiColumnListMorph >> listForSearching [

	^ super listForSearching collect: #first
]

{ #category : #accessing }
PluggableMultiColumnListMorph >> listMorphClass [
	^MulticolumnLazyListMorph
]

{ #category : #initialization }
PluggableMultiColumnListMorph >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel  wrapSelector: wrapSel [
	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	wrapSelector := wrapSel.
	self autoDeselect: true.
	self borderWidth: 1.
	self updateList.
	self selectionIndex: self getCurrentSelectionIndex.
	self initForKeystrokes
]
