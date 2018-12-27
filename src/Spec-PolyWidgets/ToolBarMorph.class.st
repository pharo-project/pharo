"
I'm a toolbar morph.
I can display toolbar items in left side or right side panels. 

"
Class {
	#name : #ToolBarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'leftPanel',
		'rightPanel',
		'displayMode',
		'toolbarItemFont'
	],
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #accessing }
ToolBarMorph class >> defaultHeight [
	^ self defaultMode height
]

{ #category : #accessing }
ToolBarMorph class >> defaultItemSeparation [
	^ 0
]

{ #category : #accessing }
ToolBarMorph class >> defaultItemSize [
	^ 50
]

{ #category : #accessing }
ToolBarMorph class >> defaultMode [
	^ ToolBarDisplayMode default
]

{ #category : #accessing }
ToolBarMorph class >> statusBarHeight [
	
	^ 25
]

{ #category : #accessing }
ToolBarMorph >> addItem: aToolbarItem [
	
	self addItem: aToolbarItem position: ToolBarItemPosition left
]

{ #category : #accessing }
ToolBarMorph >> addItem: aToolbarItem position: aPosition [

	aPosition addItem: aToolbarItem into: self.
	self refresh.
]

{ #category : #accessing }
ToolBarMorph >> addItemLeft: aMorph [

	leftPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #accessing }
ToolBarMorph >> addItemRight: aMorph [

	rightPanel addMorphBack: (aMorph
		vResizing: #spaceFill;
		yourself)
]

{ #category : #'as yet unclassified' }
ToolBarMorph >> addItemsFromContext: aContext [
	| menu |
	
	menu := CmdMenu activatedBy: ITToolbarActivation.
	menu buildInContext: aContext.
	menu buildToolbarOn: self

]

{ #category : #accessing }
ToolBarMorph >> color [
	^ color ifNil: [ Color transparent ]
]

{ #category : #accessing }
ToolBarMorph >> color: aColor [
	color := aColor
]

{ #category : #accessing }
ToolBarMorph >> displayMode [
	^ displayMode ifNil: [ displayMode := ToolBarDisplayMode default ]
]

{ #category : #accessing }
ToolBarMorph >> displayMode: anObject [
	displayMode := anObject
]

{ #category : #initialization }
ToolBarMorph >> initialize [
	super initialize.

	self
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		color: self color.
	
	self initializePanels
]

{ #category : #initialization }
ToolBarMorph >> initializePanels [
	leftPanel := self newPanelMorph.
	rightPanel := self newPanelMorph
		listDirection: #rightToLeft;
		hResizing: #spaceFill.

	self
		addMorphBack: leftPanel;
		addMorphBack: rightPanel

]

{ #category : #'private accessing' }
ToolBarMorph >> leftPanel [
	^ leftPanel
]

{ #category : #'private accessing' }
ToolBarMorph >> leftPanel: anObject [
	leftPanel := anObject
]

{ #category : #'private factory' }
ToolBarMorph >> newPanelMorph [
	^ PanelMorph new 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		cellInset: self class defaultItemSeparation@0;
		color: self color;
		yourself
]

{ #category : #'private factory' }
ToolBarMorph >> newToolbarItemFont [
	| buttonFont |

	buttonFont := Smalltalk ui theme buttonFont.
	^ LogicalFont 
		familyName: buttonFont familyName 
		pointSize: buttonFont pointSize - 3
]

{ #category : #accessing }
ToolBarMorph >> rebuildLeftPanel [
	
	self halt.
	"self leftPanel removeAllMorphs.
	leftItems do: [ :each | 
		leftPanel addMorphBack: ((each buildMorphOnToolbar: self)
			vResizing: #spaceFill;
			yourself) ]"
]

{ #category : #accessing }
ToolBarMorph >> rebuildRightPanel [
	
	self halt.
	"self rightPanel removeAllMorphs.
	rightItems do: [ :each | 
		rightPanel addMorphBack: ((each buildMorphOnToolbar: self)
			vResizing: #spaceFill;
			yourself) ]"
]

{ #category : #accessing }
ToolBarMorph >> refresh [

	self rebuildLeftPanel.
	self rebuildRightPanel.
]

{ #category : #'private accessing' }
ToolBarMorph >> rightPanel [
	^ rightPanel
]

{ #category : #'private accessing' }
ToolBarMorph >> rightPanel: anObject [
	rightPanel := anObject
]

{ #category : #accessing }
ToolBarMorph >> toolbarItemFont [
	^ toolbarItemFont ifNil: [ toolbarItemFont := self newToolbarItemFont ]
]

{ #category : #accessing }
ToolBarMorph >> toolbarItemFont: anObject [
	toolbarItemFont := anObject
]

{ #category : #accessing }
ToolBarMorph >> toolbarItemSize [
	^ self class defaultItemSize
]
