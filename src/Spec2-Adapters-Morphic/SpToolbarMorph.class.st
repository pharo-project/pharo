"
I'm a toolbar morph.
I can display toolbar items in left side or right side panels. 

"
Class {
	#name : #SpToolbarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'leftPanel',
		'rightPanel',
		'displayMode',
		'toolbarItemFont'
	],
	#category : #'Spec2-Adapters-Morphic'
}

{ #category : #accessing }
SpToolbarMorph class >> defaultHeight [
	
	^ self defaultMode height
]

{ #category : #accessing }
SpToolbarMorph class >> defaultItemSeparation [

	^ 0
]

{ #category : #accessing }
SpToolbarMorph class >> defaultItemSize [

	^ 50
]

{ #category : #accessing }
SpToolbarMorph class >> defaultMode [

	^ SpToolbarDisplayMode default
]

{ #category : #accessing }
SpToolbarMorph class >> statusBarHeight [
	
	^ 25
]

{ #category : #accessing }
SpToolbarMorph >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: SpToolbarItemPosition left
]

{ #category : #accessing }
SpToolbarMorph >> addItem: aToolbarItem position: aPosition [

	aPosition addItem: aToolbarItem into: self.
	self width: self width + aToolbarItem width.
	self refresh
]

{ #category : #accessing }
SpToolbarMorph >> addItemLeft: aMorph [

	leftPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #accessing }
SpToolbarMorph >> addItemRight: aMorph [

	rightPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #'as yet unclassified' }
SpToolbarMorph >> addItemsFromContext: aContext [
	
	self flag: #TODO.
	"| menu |
	
	menu := CmdMenu activatedBy: ITToolbarActivation.
	menu buildInContext: aContext.
	menu buildToolbarOn: self"

]

{ #category : #updating }
SpToolbarMorph >> clearItems [
	
	leftPanel removeAllMorphs.
	rightPanel removeAllMorphs.
]

{ #category : #accessing }
SpToolbarMorph >> color [
	^ color ifNil: [ Color transparent ]
]

{ #category : #accessing }
SpToolbarMorph >> color: aColor [
	color := aColor
]

{ #category : #accessing }
SpToolbarMorph >> displayMode [
	^ displayMode ifNil: [ displayMode := SpToolbarDisplayMode default ]
]

{ #category : #accessing }
SpToolbarMorph >> displayMode: anObject [
	displayMode := anObject
]

{ #category : #initialization }
SpToolbarMorph >> initialize [
	super initialize.

	self
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: self color;
		extent: 0@0.
	
	self initializePanels
]

{ #category : #initialization }
SpToolbarMorph >> initializePanels [

	leftPanel := self newPanelMorph.
	rightPanel := self newPanelMorph
		listDirection: #rightToLeft;
		hResizing: #spaceFill.

	self
		addMorphBack: leftPanel;
		addMorphBack: rightPanel

]

{ #category : #'private accessing' }
SpToolbarMorph >> leftPanel [
	^ leftPanel
]

{ #category : #'private accessing' }
SpToolbarMorph >> leftPanel: anObject [
	leftPanel := anObject
]

{ #category : #'private factory' }
SpToolbarMorph >> newPanelMorph [

	^ PanelMorph new 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: self class defaultItemSeparation@0;
		extent: 0@0;
		color: self color;
		yourself
]

{ #category : #'private factory' }
SpToolbarMorph >> newToolbarItemFont [
	| buttonFont |

	buttonFont := self theme buttonFont.
	^ LogicalFont 
		familyName: buttonFont familyName 
		pointSize: buttonFont pointSize - 3
]

{ #category : #accessing }
SpToolbarMorph >> refresh [

	self flag: #TODO. "Maybe remove?"
]

{ #category : #'private accessing' }
SpToolbarMorph >> rightPanel [
	^ rightPanel
]

{ #category : #'private accessing' }
SpToolbarMorph >> rightPanel: anObject [
	rightPanel := anObject
]

{ #category : #accessing }
SpToolbarMorph >> toolbarItemFont [
	^ toolbarItemFont ifNil: [ toolbarItemFont := self newToolbarItemFont ]
]

{ #category : #accessing }
SpToolbarMorph >> toolbarItemFont: anObject [
	toolbarItemFont := anObject
]

{ #category : #accessing }
SpToolbarMorph >> toolbarItemSize [

	^ self displayMode height
]
