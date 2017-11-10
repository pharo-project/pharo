"
A GLMTabSelectorBrick is a simplified TabSelectorMorph with support of smart tab resizing.
New implementation doesn't have any scrollbars or scroll arrows.

Instance Variables
	controls:		<Object>
	font:		<Object>
	selectedIndex:		<Object>
	tabs:		<Object>
	visibleTabRange:		<Object>
	wrapScrolling:		<Object>

controls
	- xxxxx

font
	- xxxxx

selectedIndex
	- xxxxx

tabs
	- xxxxx

visibleTabRange
	- xxxxx

wrapScrolling
	- xxxxx

"
Class {
	#name : #GLMTabSelectorBrick,
	#superclass : #GLMFoundationBrick,
	#instVars : [
		'selectedIndex',
		'font',
		'tabs'
	],
	#category : #Glamour-Morphic-Brick-Morphic
}

{ #category : #adding/removing }
GLMTabSelectorBrick >> addTab: aStringOrMorph [
	"Add a new tab with the given text."
	
	self tabs add: (self newLabelBrick: aStringOrMorph).
	self updateTabs
]

{ #category : #adding/removing }
GLMTabSelectorBrick >> addTab: aStringOrMorph selected: selectedStringOrMorph [
	"Add a new tab with the given text or morph and alternate for when selected."
	
	self tabs add: (self newLabelBrick: aStringOrMorph selected: selectedStringOrMorph).
	self updateTabs
]

{ #category : #accessing }
GLMTabSelectorBrick >> font [
	"Answer the label font"
	^ font
]

{ #category : #accessing }
GLMTabSelectorBrick >> font: aFont [
	"Set the label font"
	font := aFont
]

{ #category : #'event handling' }
GLMTabSelectorBrick >> handlesKeyboard: evt [
	"Yes, we do it here."
	
	^ true
]

{ #category : #'event handling' }
GLMTabSelectorBrick >> keyStroke: event [ 
	"Process keys navigation and space to toggle."

	(self navigationKey: event) ifTrue: [ ^ self ].
	event keyCharacter = Character arrowLeft
		ifTrue: [ self selectPreviousTab ].
	event keyCharacter = Character arrowRight
		ifTrue: [ self selectNextTab ]
]

{ #category : #'event handling' }
GLMTabSelectorBrick >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #'instance creation' }
GLMTabSelectorBrick >> newLabelBrick: aStringOrBrick [
	"Answer a new label brick with the given label text."

	^ GLMTabLabelBrick new
		label: aStringOrBrick;
		when: #onClicked send: #onTabClicked:with: to: self;
		tabSelector: self;
		vSpaceFill
]

{ #category : #'instance creation' }
GLMTabSelectorBrick >> newLabelBrick: aStringOrBrick selected: selectedStringOrBrick [
	"Answer a new label brick with the given label text or brick and
	alternate when selected."
	^ GLMTabLabelBrick new
		label: aStringOrBrick selected: selectedStringOrBrick;
		when: #onClicked send: #onTabClicked:with: to: self;
		tabSelector: self;
		vSpaceFill
]

{ #category : #'event handling' }
GLMTabSelectorBrick >> onTabClicked: anEvent with: aBrick [ 
	"A tab has been clicked."
	
	self selectedIndex: (self tabs indexOf: aBrick)
]

{ #category : #actions }
GLMTabSelectorBrick >> relabelTab: aTab with: aStringOrBrick [
	"Relabel the tab update tab layout."

	aTab label: aStringOrBrick.
	self updateTabs
]

{ #category : #adding/removing }
GLMTabSelectorBrick >> removeTabIndex: anInteger [
	"Remove the tab at the given index and
	adjust any selected (next or first if was last)."
	
	self tabs removeAt: anInteger.
	self selectedIndex: 0.
	self tabs ifNotEmpty: [
		self selectedIndex: self selectedIndex - 1 \\ self tabs size + 1]
		
]

{ #category : #actions }
GLMTabSelectorBrick >> selectNextTab [
	"Select the next tab, or the first if none selected."
	
	self selectedIndex: self selectedIndex \\ self tabs size + 1
]

{ #category : #actions }
GLMTabSelectorBrick >> selectPreviousTab [
	"Select the previous tab, or the last if none selected."
	
	self selectedIndex: (self selectedIndex < 2
		ifTrue: [self tabs size]
		ifFalse: [self selectedIndex - 1])
]

{ #category : #accessing }
GLMTabSelectorBrick >> selectedIndex [
	"Answer the value of selectedIndex"

	^ selectedIndex ifNil: [ selectedIndex := 0 ]
]

{ #category : #accessing }
GLMTabSelectorBrick >> selectedIndex: index [
	"Set the value of selectedIndex"

	|oldIndex|
	self selectedIndex == index ifTrue: [^ self ].
	oldIndex := self selectedIndex.
	selectedIndex := (index min: self tabs size).
	self updateTabs.
	self changed: #selectedIndex with: oldIndex
]

{ #category : #accessing }
GLMTabSelectorBrick >> selectedTab [
	"Answer the selected tab."
	
	^ self selectedIndex = 0
		ifFalse: [self tabs at: self selectedIndex ifAbsent: [ nil ] ]
]

{ #category : #accessing }
GLMTabSelectorBrick >> tabs [

	^ tabs ifNil: [ tabs := OrderedCollection new ]
]

{ #category : #accessing }
GLMTabSelectorBrick >> tabs: anObject [

	tabs := anObject
]

{ #category : #actions }
GLMTabSelectorBrick >> updateTabs [
	"Update the submorphs based on visible tabs."

	|selectedTab|
	self removeAllBricks.
	self tabs ifEmpty: [ ^ self ].
	selectedTab := self selectedTab.
	self tabs do: [:t |
		t selected: (t == selectedTab).
		t hSpaceFill: (100 / self tabs size) asFloat ].
	self
		addAllBricks: self tabs
]
