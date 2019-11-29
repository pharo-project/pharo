"
I'm a toolbar morph.
I can display toolbar items in left side or right side panels. 

"
Class {
	#name : #SpToolBarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'leftPanel',
		'rightPanel',
		'displayMode',
		'toolbarItemFont'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpToolBarMorph class >> defaultHeight [
	
	^ self defaultMode height
]

{ #category : #accessing }
SpToolBarMorph class >> defaultItemSeparation [

	^ 0
]

{ #category : #accessing }
SpToolBarMorph class >> defaultItemSize [

	^ 50
]

{ #category : #accessing }
SpToolBarMorph class >> defaultMode [

	^ SpToolBarDisplayMode default
]

{ #category : #accessing }
SpToolBarMorph class >> statusBarHeight [
	
	^ 25
]

{ #category : #accessing }
SpToolBarMorph >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: SpToolBarItemPosition left
]

{ #category : #accessing }
SpToolBarMorph >> addItem: aToolbarItem position: aPosition [

	aPosition addItem: aToolbarItem into: self.
	self width: self width + aToolbarItem width.
	self refresh
]

{ #category : #accessing }
SpToolBarMorph >> addItemLeft: aMorph [

	leftPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #accessing }
SpToolBarMorph >> addItemRight: aMorph [

	rightPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #'as yet unclassified' }
SpToolBarMorph >> addItemsFromContext: aContext [
	
	self flag: #TODO.
	"| menu |
	
	menu := CmdMenu activatedBy: ITToolbarActivation.
	menu buildInContext: aContext.
	menu buildToolbarOn: self"

]

{ #category : #updating }
SpToolBarMorph >> clearItems [
	
	leftPanel removeAllMorphs.
	rightPanel removeAllMorphs.
]

{ #category : #accessing }
SpToolBarMorph >> color [
	^ color ifNil: [ Color transparent ]
]

{ #category : #accessing }
SpToolBarMorph >> color: aColor [
	color := aColor
]

{ #category : #accessing }
SpToolBarMorph >> displayMode [
	^ displayMode ifNil: [ displayMode := SpToolBarDisplayMode default ]
]

{ #category : #accessing }
SpToolBarMorph >> displayMode: anObject [
	displayMode := anObject
]

{ #category : #initialization }
SpToolBarMorph >> initialize [
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
SpToolBarMorph >> initializePanels [

	leftPanel := self newPanelMorph.
	rightPanel := self newPanelMorph
		listDirection: #rightToLeft;
		hResizing: #spaceFill.

	self
		addMorphBack: leftPanel;
		addMorphBack: rightPanel

]

{ #category : #'private accessing' }
SpToolBarMorph >> leftPanel [
	^ leftPanel
]

{ #category : #'private accessing' }
SpToolBarMorph >> leftPanel: anObject [
	leftPanel := anObject
]

{ #category : #'private factory' }
SpToolBarMorph >> newPanelMorph [

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
SpToolBarMorph >> newToolbarItemFont [
	| buttonFont |

	buttonFont := self theme buttonFont.
	^ LogicalFont 
		familyName: buttonFont familyName 
		pointSize: buttonFont pointSize - 3
]

{ #category : #accessing }
SpToolBarMorph >> refresh [

	self flag: #TODO. "Maybe remove?"
]

{ #category : #'private accessing' }
SpToolBarMorph >> rightPanel [
	^ rightPanel
]

{ #category : #'private accessing' }
SpToolBarMorph >> rightPanel: anObject [
	rightPanel := anObject
]

{ #category : #accessing }
SpToolBarMorph >> toolbarItemFont [
	^ toolbarItemFont ifNil: [ toolbarItemFont := self newToolbarItemFont ]
]

{ #category : #accessing }
SpToolBarMorph >> toolbarItemFont: anObject [
	toolbarItemFont := anObject
]

{ #category : #accessing }
SpToolBarMorph >> toolbarItemSize [

	^ self displayMode height
]
