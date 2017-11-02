"
I'm a morph where you can drag windows to group them as tabs.

Try:

(GroupWindowMorph new openInWindowLabeled: 'Window organizer') extent: 400@400.
"
Class {
	#name : #GroupWindowMorph,
	#superclass : #Morph,
	#traits : 'TEasilyThemed',
	#classTraits : 'TEasilyThemed classTrait',
	#instVars : [
		'tabGroup'
	],
	#category : 'Morphic-Widgets-Windows-Widgets'
}

{ #category : #layout }
GroupWindowMorph >> acceptDroppingMorph: aSystemWindow event: evt [
	"Add the window."

	self addWindow: aSystemWindow
]

{ #category : #windows }
GroupWindowMorph >> addWindow: aSystemWindow [
  "Add an existing window to the pages."

	aSystemWindow isTopWindow
		ifTrue: [SystemWindow passivateTopWindow].
		
	self tabGroup
		addPage: aSystemWindow configureForEmbedding
		label: (self tabLabelFor: aSystemWindow);
		selectedPageIndex: self tabGroup pages size.
		
	self isActive ifFalse: [self tabGroup selectedTab passivate].
	
	self tabGroup selectedTab
		on: #startDrag
		send: #dragTab:event:in:
		to: self
		withValue: aSystemWindow.
		
	aSystemWindow announcer
		when: WindowLabelled 
		send: #onWindowLabelChanged:
		to: self
]

{ #category : #layout }
GroupWindowMorph >> changePropotionalLayout [
	| layout |
	((layout := self layoutPolicy) notNil and:[layout isProportionalLayout])
		ifTrue:[^self]. "already proportional layout"
	self layoutPolicy: ProportionalLayout new.
	self layoutChanged.
]

{ #category : #initialize }
GroupWindowMorph >> defaultColor [
	^ self theme windowColor lighter
]

{ #category : #dropping/grabbing }
GroupWindowMorph >> dragTab: aSystemWindow event: anEvent in: aTabLabel [
	"Drag a tab. Remove the window from the organiser and place in hand."
	
	self removeWindow: aSystemWindow.
	aSystemWindow position: anEvent targetPoint.
	anEvent hand grabMorph: aSystemWindow
]

{ #category : #windows }
GroupWindowMorph >> grabWindow [
	"Request an existing window from the user and add it."

	|windows choice|
	windows := self world visibleSystemWindows.
	choice := UIManager default
		chooseFrom: (windows collect: [:e | e labelString])
		values: windows
		lines: #()
		message: 'Choose a window to add to the organiser' translated
		title: 'Grab window' translated.
	choice ifNotNil: [self addWindow: choice]
]

{ #category : #events-processing }
GroupWindowMorph >> handleDropMorph: anEvent [
	"Handle a dropping morph."
	
	| aMorph |
	aMorph := anEvent contents.
	"Ignore whether the dropping morph wants to be dropped, just whether the receiver wants it"
	(self wantsDroppedMorph: aMorph event: anEvent)
		ifFalse: [^ self].
	anEvent wasHandled: true.
	self acceptDroppingMorph: aMorph event: anEvent.
	aMorph justDroppedInto: self event: anEvent

]

{ #category : #initialization }
GroupWindowMorph >> initialize [
	"Add the tab group with an inital workspace."

	super initialize.
	self changeProportionalLayout.
	self tabGroup: self newTabGroup.
	self tabGroup tabSelectorMorph addDependent: self.
	self
		dropEnabled: true;
		addMorph: self tabGroup fullFrame: LayoutFrame identity.
	self tabGroup color: Color transparent.
	
]

{ #category : #testing }
GroupWindowMorph >> isActive [
	^ false
]

{ #category : #testing }
GroupWindowMorph >> isWindowActive: aSystemWindow [
	"Answer whether the given window is active.
	True if the receiver is active and the window is the
	current page."
	
	^ self tabGroup page == aSystemWindow and: [aSystemWindow isTopWindow ]
]

{ #category : #windows }
GroupWindowMorph >> newTabGroup [
	"Answer a new tab group."

	^(self newTabGroup: #())
		cornerStyle: #square
]

{ #category : #windows }
GroupWindowMorph >> offerWindowMenu [
	"Popup the window menu. Fill from current workspace."

	| aMenu |
	aMenu := self buildWindowMenu.
	aMenu
		addLine;
		add: 'Grab window...' target: self selector: #grabWindow.
	aMenu lastItem icon: (self iconNamed: #smallWindowIcon).
	self tabGroup page
		ifNotNil: [ :page | page model addModelItemsToWindowMenu: aMenu ].
	aMenu popUpEvent: self currentEvent in: self world
]

{ #category : #windows }
GroupWindowMorph >> onWindowLabelChanged: ann [
	self tabGroup relabelPage: ann window with: (self tabLabelFor: ann window)  
]

{ #category : #windows }
GroupWindowMorph >> removeWindow: aSystemWindow [
	"Remove a window from the pages."

	aSystemWindow announcer unsubscribe: self.
	self tabGroup removePage: aSystemWindow.
	aSystemWindow configureForUnembedding.
	"self world addMorph: aSystemWindow"
	aSystemWindow delete.
	self tabGroup pages size = 0 
		ifTrue: [ self owner delete ]
]

{ #category : #accessing }
GroupWindowMorph >> tabGroup [

	^ tabGroup
]

{ #category : #accessing }
GroupWindowMorph >> tabGroup: anObject [

	tabGroup := anObject
]

{ #category : #windows }
GroupWindowMorph >> tabLabelFor: aSystemWindow [
	"Answer the tab label to use for the given page."

	^self newRow: {
		 (self newButtonLabel: (aSystemWindow labelString truncateWithElipsisTo: 40))
			setBalloonText: aSystemWindow labelString.
		 self
			newCloseControlFor: nil
			action: [self removeWindow: aSystemWindow]
			help: 'Close this tab and free the window'}
]

{ #category : #updating }
GroupWindowMorph >> update: aSymbol [
	"Handle tab changes."

	super update: aSymbol.
	aSymbol == #selectedIndex
		ifTrue: [self tabGroup page activate]
]

{ #category : #updating }
GroupWindowMorph >> update: aSymbol with: anObject [
	"Handle tab changes."

	super update: aSymbol.
	aSymbol == #selectedIndex
		ifTrue: [ |selectedPage|
				selectedPage := self tabGroup pages at: anObject ifAbsent: [nil].
				selectedPage ifNotNil: [
						selectedPage rememberKeyboardFocus: ActiveHand keyboardFocus.
						self tabGroup page ifNotNil: [self tabGroup page activate].]
				]
]

{ #category : #dropping/grabbing }
GroupWindowMorph >> wantsDroppedMorph: aMorph event: evt [
  "Accept if a SystemWindow."
  
  self visible ifFalse: [^ false].
  self dropEnabled ifFalse: [^ false].
  
  (self tabGroup tabSelectorMorph bounds containsPoint: evt position) ifFalse: [^ false].
  
  ^aMorph isSystemWindow
]
