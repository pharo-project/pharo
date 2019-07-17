"
I am a bar gathering tabs. I am the one managing the selected tab and related bahavior (unselecting the last one, updating the layout to make the selected tab on top of the other)
"
Class {
	#name : #TabBarMorph,
	#superclass : #PanelMorph,
	#instVars : [
		'tabs',
		'selectionHistory',
		'menuButton',
		'numberOfSelectedTabs'
	],
	#category : #'Morphic-Widgets-Tabs'
}

{ #category : #'private-constants' }
TabBarMorph >> actionOffset [
	^ 4 * self displayScaleFactor
]

{ #category : #private }
TabBarMorph >> actionsChanged: aTab [

	self selectedTab = aTab ifFalse: [ ^ self ].
	self adjustLayout.
	
]

{ #category : #private }
TabBarMorph >> addActionsFor: aTab [
	| rightOffset |
	
	"This check should maybe not be there but without it the CI fails a lot. Maybe it is due to a race condition?"
	aTab ifNil: [ ^ self ].

	rightOffset := self actionOffset.
	
	aTab hasMenu
		ifTrue: [ 
			rightOffset := 2 * rightOffset + self menuButtonWidth.
			self addMenuButton ].
		
	aTab actions reverseDo: [ :each || topOffset |
		topOffset := self height - each icon height // 2.
		self 
			addMorph: (self createActionButtonFor: each)
			fullFrame: (LayoutFrame identity
				bottomFraction: 0;
				leftFraction: 1;
				topOffset: topOffset;
				bottomOffset: topOffset + each icon height;
				rightOffset: rightOffset negated ;
				leftOffset: rightOffset negated - each icon width ).
				
		rightOffset := rightOffset + each icon width + self actionOffset]
]

{ #category : #'private-history' }
TabBarMorph >> addFirstInHistory: aTab [

	self removeFromHistory: aTab.
	selectionHistory addFirst: aTab
]

{ #category : #'private-history' }
TabBarMorph >> addInHistory: aTab [

	self removeFromHistory: aTab.
	selectionHistory addLast: aTab
]

{ #category : #initialization }
TabBarMorph >> addMenuButton [
	| topOffset rightOffset |
	
	rightOffset := self actionOffset.
	topOffset := (self height - 16 * self displayScaleFactor) // 2.
	self
		addMorph: menuButton
		fullFrame:
			(LayoutFrame identity
				bottomFraction: 0;
				leftFraction: 1;
				rightOffset: rightOffset negated;
				leftOffset: rightOffset negated - self menuButtonWidth;
				topOffset: topOffset;
				bottomOffset: topOffset + self menuButtonHeight)
]

{ #category : #protocol }
TabBarMorph >> addTab: aTab [
	
	(tabs includes: aTab) ifTrue: [ ^ self ].

	tabs add: aTab.

	self registerActionsFor: aTab.
	self selectedTab ifNil: [ aTab selected: true ].
	self addFirstInHistory: aTab.
	self adjustLayout.
	self changed.
	self triggerEvent: #tabAdded with: aTab
]

{ #category : #drawing }
TabBarMorph >> adjustLayout [
	| length overlap |
	
	length := self computeLength.
	self removeAllMorphs.
	length isZero ifTrue: [ ^ self ].
	overlap := self overlap.	
	
	selectionHistory do: [ :tab || index |
		index := (tabs indexOf: tab) - 1.
		self 
			addMorph: tab
			fullFrame: (LayoutFrame identity
				rightFraction: 0;
				leftOffset: (index*(length-overlap));
				rightOffset: (index*(length-overlap)) + length;
				bottomOffset: -1;
				yourself ).
		tab updateShowIcon ].
	
	self addActionsFor: self selectedTab.
]

{ #category : #drawing }
TabBarMorph >> borderColor [
	"I do not use #borderColor because I want a light border"
	^ self theme lightBackgroundColor
]

{ #category : #accessing }
TabBarMorph >> bounds: b [

	super bounds: b.
	self adjustLayout
]

{ #category : #protocol }
TabBarMorph >> canBeClosed [

	^ tabs allSatisfy: [ :tab | tab closeable ]
]

{ #category : #protocol }
TabBarMorph >> closeAllTabs [

	tabs copy do: [ :tab | tab close ]
]

{ #category : #private }
TabBarMorph >> computeLength [
	| width size length |

	size := tabs size.
	size isZero ifTrue: [ ^ 0 ].
	width := self width - self extraSpace + ((size-1) * self overlap).
	
	length := width / size.
	length := length min: 150 * self displayScaleFactor.
	length := length max: 33 * self displayScaleFactor.

	^ length
	
]

{ #category : #private }
TabBarMorph >> createActionButtonFor: anAction [

	^ TabActionButtonMorph forAction: anAction
]

{ #category : #private }
TabBarMorph >> createMenuButton [
	"Answer a button for the window menu."
	| form msb |
	
	form := self theme windowMenuForm.
	msb := MultistateButtonMorph new extent: form extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	
	form := self theme windowMenuPassiveForm.
	msb extent: form extent.
	msb activeDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	
	form := self theme windowMenuForm.
	msb extent: form extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverUpFillStyle: (ImageFillStyle form: form).
	
	form := self theme windowMenuPassiveForm.
	msb
		extent: form extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverDownFillStyle: (ImageFillStyle form: form);
		addUpAction: [ self popUpMenu ];
		setBalloonText: 'tab menu' translated;
		extent: 16@16.
		
	^ msb
]

{ #category : #protocol }
TabBarMorph >> delete [

	super delete.
	self triggerEvent: #barDeleted with: self
]

{ #category : #'private-actions' }
TabBarMorph >> deleteSelectedTabs [
	
	self selectedTabs do: [:e | e close ].
]

{ #category : #drawing }
TabBarMorph >> drawLinesOn: canvas [
	"This method draws the line below the tab-label, to make it look as 'behind' (when there is one 
	 selected, it draws that line except on the corresponding selected tab."
	self selectedTab 
		ifNotNil: [ self drawWithSelectionOn: canvas ]	
		ifNil: [ self drawWithoutSelectedOn: canvas ]

]

{ #category : #drawing }
TabBarMorph >> drawSubmorphsOn: canvas [
	super drawSubmorphsOn: canvas.
	self drawLinesOn: canvas
]

{ #category : #drawing }
TabBarMorph >> drawWithSelectionOn: canvas [
	canvas
		line: self bottomLeft + (0 @ -1)
		to: self selectedTab bottomLeft
		width: 1
		color: self borderColor.
	canvas
		line: self selectedTab bottomLeft
		to: self selectedTab  bottomRight
		width: 1
		color: self selectedColor.
	canvas
		line: self selectedTab  bottomRight
		to: self bottomRight + (0 @ -1)
		width: 1
		color: self borderColor
]

{ #category : #drawing }
TabBarMorph >> drawWithoutSelectedOn: canvas [
	canvas
		line: self bottomLeft + (0 @ -1)
		to: self bottomRight + (0 @ -1)
		width: 1
		color: self borderColor
]

{ #category : #private }
TabBarMorph >> extraSpace [

	^ tabs max: [ :each | each extraSpaceForActions ]
]

{ #category : #initialization }
TabBarMorph >> initialize [

	super initialize.

	tabs := OrderedCollection new.
	selectionHistory := OrderedCollection new.
	menuButton := self createMenuButton.
	numberOfSelectedTabs := 1.
	
	self changeProportionalLayout.
	self addMenuButton.
	
]

{ #category : #protocol }
TabBarMorph >> isMultiSelection [

	^ numberOfSelectedTabs > 1
]

{ #category : #'private-constants' }
TabBarMorph >> menuButtonHeight [
	^ 16 * self displayScaleFactor
]

{ #category : #'private-constants' }
TabBarMorph >> menuButtonWidth [
	^ 16 * self displayScaleFactor
]

{ #category : #private }
TabBarMorph >> needSpaceForActions [ 
	| tab |
	
	tab := self selectedTab.
	tab ifNil: [ ^ false ].

	^ self selectedTab hasMenu or: [ self selectedTab hasActions ]
]

{ #category : #protocol }
TabBarMorph >> orderedSelectedTabs [
	
	^ tabs select: [ :e | e selected ]
]

{ #category : #private }
TabBarMorph >> overlap [

	^ 8 * self displayScaleFactor
]

{ #category : #'change reporting' }
TabBarMorph >> ownerChanged [

	super ownerChanged.
	self adjustLayout
]

{ #category : #menu }
TabBarMorph >> popUpMenu [
	| menu |
	
	menu := self selectedTab menu.
	menu popUpAt: menuButton bottomRight forHand: ActiveHand in: self currentWorld
]

{ #category : #private }
TabBarMorph >> registerActionsFor: aTab [

	aTab 
		when: #tabSelected send: #tabSelected: to: self;
		when: #tabDeleted send: #tabDeleted: to: self;
		when: #rightKeyPressed send: #rightKeyPressed: to: self;
		when: #leftKeyPressed send: #leftKeyPressed: to: self;
		when: #tabRefreshed send: #tabRefreshed: to: self;
		when: #actionsChanged send: #actionsChanged: to: self;
		when: #tabResetSelection send: #tabResetSelection: to: self;
		when: #tabAddedToSelection send: #tabAddedToSelection: to: self;
		when: #tabRemovedFromSelection send: #tabRemovedFromSelection: to: self;
		when: #tabSelectTo send: #tabSelectTo: to: self;
		when: #tabEmptyContents send: #tabEmptyContents: to: self
]

{ #category : #'private-history' }
TabBarMorph >> removeFromHistory: aTab [

	selectionHistory remove: aTab ifAbsent: [  ].
]

{ #category : #'private-actions' }
TabBarMorph >> removeTab: aTab [
	| nextSelection |

	(aTab selected and: [selectionHistory size > 1])
		ifTrue: [ 
			nextSelection := selectionHistory at: selectionHistory size - 1.
			nextSelection selected: true ].
		
	self removeFromHistory: aTab.
	tabs remove: aTab.

	self adjustLayout
]

{ #category : #protocol }
TabBarMorph >> reversedSelectedTabs [

	^ selectionHistory last: numberOfSelectedTabs
]

{ #category : #protocol }
TabBarMorph >> selectLastTab [
	
	tabs ifEmpty: [ ^self ].
	tabs last selected: true
]

{ #category : #'private-actions' }
TabBarMorph >> selectNext [
	| index nextSelection |
	
	index := tabs indexOf: self selectedTab.
	nextSelection := tabs at: index + 1 ifAbsent: [ ^ self ].

	nextSelection selected: true.
]

{ #category : #'private-actions' }
TabBarMorph >> selectPrevious [
	| index nextSelection |
	
	index := tabs indexOf: self selectedTab.
	nextSelection := tabs at: index - 1 ifAbsent: [ ^ self ].

	nextSelection selected: true.
]

{ #category : #protocol }
TabBarMorph >> selectTabAt: index ifAbsent: aBlock [
	| tab |
	
	tab := tabs at: index ifAbsent: [ ^ aBlock value ].
	tab selected: true
]

{ #category : #drawing }
TabBarMorph >> selectedColor [
	^ self theme selectionColor
]

{ #category : #accessing }
TabBarMorph >> selectedTab [

	  ^selectionHistory ifEmpty: [ nil ] ifNotEmpty: [ selectionHistory last ]
]

{ #category : #protocol }
TabBarMorph >> selectedTabs [

	^ (selectionHistory last: numberOfSelectedTabs) reversed
]

{ #category : #'private-actions' }
TabBarMorph >> tabAddedToSelection: aTab [

	aTab selected ifTrue: [ ^ self ].
	numberOfSelectedTabs := numberOfSelectedTabs + 1.
	self addInHistory: aTab.
	aTab silentlySelected: true.
	self adjustLayout.
	
	self triggerEvent: #tabAddedToSelection with: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabDeleted: aTab [

	self removeTab: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabEmptyContents: aTab [

	self selectedTab = aTab ifFalse: [ ^ self ].

	(selectionHistory last: numberOfSelectedTabs) do: [ :each | each == aTab ifFalse: [ each silentlySelected: false ] ].	
	numberOfSelectedTabs := 1.
	self adjustLayout.
	
	self triggerEvent: #tabEmptyContents with: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabRefreshed: aTab [

	self selectedTab = aTab ifFalse: [ ^ self ].

	self triggerEvent: #tabRefreshed with: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabRemovedFromSelection: aTab [

	numberOfSelectedTabs == 1 ifTrue: [ ^ self ].
	numberOfSelectedTabs := numberOfSelectedTabs - 1.

	"Insert the remove tab just before the selected ones to preserve order"
	selectionHistory remove: aTab.
	selectionHistory add: aTab afterIndex: (selectionHistory size - numberOfSelectedTabs).
	
	aTab silentlySelected: false.
	self adjustLayout.
	
	self triggerEvent: #tabRemovedFromSelection with: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabResetSelection: aTab [
	"Clicked on an already clicked tab, since it can happened after a multi selection, I have to clear the selection except for this tab"
	
	selectionHistory ifNotEmpty: [
		(selectionHistory last: numberOfSelectedTabs) do: [ :each | each == aTab ifFalse: [ each silentlySelected: false ] ] ].
	
	numberOfSelectedTabs := 1.
	
	self addInHistory: aTab.
	self adjustLayout.
	
	self triggerEvent: #tabResetSelection with: aTab
]

{ #category : #'private-actions' }
TabBarMorph >> tabSelectTo: aTab [
	| index selectedIndex |
	
	index := tabs indexOf: aTab. "If absent, then the system must be in a curious shape"
	selectedIndex := tabs indexOf: self selectedTab ifAbsent: [ ^ self ]. "No currently selected tab"
	
	index compareWith: selectedIndex
		ifLesser: [ 
			selectedIndex to: index by: -1 do: [ :i | 
				(tabs at: i) addToSelection	] ]
		ifEqual: [  ]
		ifGreater: [ 
			selectedIndex to: index do: [ :i | 
				(tabs at: i) addToSelection	] ].
			
	self addInHistory: aTab.
	self adjustLayout.
]

{ #category : #'private-actions' }
TabBarMorph >> tabSelected: aTab [

	self isMultiSelection
		ifTrue: [ self tabResetSelection: aTab ]
		ifFalse: [ 
			self selectedTab ifNotNil: [ self selectedTab selected: false ].
			self addInHistory: aTab.	
			self adjustLayout.
			self triggerEvent: #tabSelected with: aTab ]
]

{ #category : #accessing }
TabBarMorph >> tabs [
	^ tabs
]

{ #category : #protocol }
TabBarMorph >> tabs: aCollection [

	tabs do: [ :e | e silentlySelected: false ].
	tabs := aCollection.
	selectionHistory removeAll.
	
	aCollection do: [ :aTab |
		self registerActionsFor: aTab.
		self selectedTab ifNil: [ aTab selected: true ].
		self addFirstInHistory: aTab ].
	
	self adjustLayout.
	self changed.
	self triggerEvent: #tabsChanged
]

{ #category : #initialization }
TabBarMorph >> themeChanged [
	super themeChanged.
	tabs do:#themeChanged.
]

{ #category : #accessing }
TabBarMorph >> useSortedTabsBy: sortBlock [
	tabs := SortedCollection sortBlock: sortBlock
]
