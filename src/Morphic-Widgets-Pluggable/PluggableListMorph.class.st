"
...

When a PluggableListMorph is in focus, type in a letter (or several
letters quickly) to go to the next item that begins with that letter.
Special keys (up, down, home, etc.) are also supported.
"
Class {
	#name : #PluggableListMorph,
	#superclass : #ScrollPane,
	#instVars : [
		'list',
		'getListSelector',
		'getListSizeSelector',
		'getListElementSelector',
		'getIndexSelector',
		'setIndexSelector',
		'keystrokeActionSelector',
		'autoDeselect',
		'lastKeystrokeTime',
		'lastKeystrokes',
		'doubleClickSelector',
		'handlesBasicKeys',
		'potentialDropRow',
		'listMorph',
		'hScrollRangeCache',
		'dragItemSelector',
		'dropItemSelector',
		'wantsDropSelector',
		'wrapSelector',
		'searchedElement',
		'multipleSelection',
		'dragOnOrOff',
		'setSelectionListSelector',
		'getSelectionListSelector',
		'resetListSelector',
		'keystrokeSelector',
		'backgroundColoringBlockOrSelector',
		'separatorBlockOrSelector',
		'separatorSize',
		'separatorColor',
		'lastNonZeroIndex',
		'canMove',
		'selectionColor',
		'selectionColorToUse',
		'enabled',
		'getEnabledSelector'
	],
	#category : #'Morphic-Widgets-Pluggable-Lists'
}

{ #category : #'instance creation' }
PluggableListMorph class >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel [
	^ self new
		on: anObject
		list: listSel
		primarySelection: getSelectionSel
		changePrimarySelection: setSelectionSel
		listSelection: getListSel
		changeListSelection: setListSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default"
]

{ #category : #'instance creation' }
PluggableListMorph class >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel keystroke: keyActionSel [ 
	^ self new
		on: anObject
		list: listSel
		primarySelection: getSelectionSel
		changePrimarySelection: setSelectionSel
		listSelection: getListSel
		changeListSelection: setListSel
		menu: getMenuSel
		keystroke: keyActionSel
]

{ #category : #'instance creation' }
PluggableListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: nil
		keystroke: #arrowKey:from:		"default"
]

{ #category : #'instance creation' }
PluggableListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default"

]

{ #category : #'instance creation' }
PluggableListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel

]

{ #category : #'drag and drop' }
PluggableListMorph >> acceptDroppingMorph: aMorph event: evt [ 
	"This message is sent when a morph is dropped onto a morph that has     
	agreed to accept the dropped morph by responding 'true' to the     
	wantsDroppedMorph:Event: message. The default implementation just     
	adds the given morph to the receiver."
	"Here we let the model do its work."

	dropItemSelector 
		ifNotNil: [| item |
			dropItemSelector ifNil:[^self].
			item := aMorph passenger.
			model perform: dropItemSelector with: item with: potentialDropRow ]
		ifNil: [ 
			self model
				acceptDroppingMorph: aMorph
				event: evt
				inMorph: self ].			
	self resetPotentialDropRow.
	evt hand releaseMouseFocus: self.
	Cursor normal show.


]

{ #category : #menus }
PluggableListMorph >> addCustomMenuItems:  aMenu hand: aHandMorph [
	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'list font...' target: self selector: #setListFont.
	aMenu add: 'copy list to clipboard' target: self selector: #copyListToClipboard.
	aMenu add: 'copy selection to clipboard' target: self selector: #copySelectionToClipboard
]

{ #category : #drawing }
PluggableListMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self selectionColor: self selectionColor.
	self fillStyle: self fillStyleToUse.
	self borderWidth > 0 ifTrue: [
		self borderStyle: self borderStyleToUse]
]

{ #category : #'submorphs-accessing' }
PluggableListMorph >> allSubmorphNamesDo: nameBlock [
	"Assume list morphs do not have named parts -- saves MUCH time"

	^ self
]

{ #category : #accessing }
PluggableListMorph >> autoDeselect [

	^ autoDeselect
		ifNil: [ self resetListSelector notNil ]
		ifNotNil: [ autoDeselect ]
]

{ #category : #initialization }
PluggableListMorph >> autoDeselect: trueOrFalse [
	"Enable/disable autoDeselect (see class comment)"
	autoDeselect := trueOrFalse.
]

{ #category : #'background coloring' }
PluggableListMorph >> backgroundColorFor: aRow [

	| return |
	aRow ifNil: [ ^ nil ].
	self enabled
		ifFalse: [ return := Color white darker darker ].
	self backgroundColoringBlockOrSelector
		ifNotNil: [:blockOrSelector || anItem |
			anItem := getListElementSelector
						ifNil: [ list at: aRow ifAbsent: [ ^ nil ]]
						ifNotNil: [ model perform: getListElementSelector with: aRow ].

			return := blockOrSelector isBlock
				ifTrue: [ blockOrSelector cull: anItem cull: aRow ]
				ifFalse: [
					blockOrSelector isSymbol
						ifTrue: [ blockOrSelector numArgs == 0
									ifTrue: [ anItem perform: blockOrSelector ]
									ifFalse: [ self model perform: blockOrSelector withEnoughArguments: { anItem. aRow} ]]
						ifFalse: [ nil ]]].
		
		^ return isColor
			ifTrue: [ return ]
			ifFalse: [ nil ]
]

{ #category : #'background coloring' }
PluggableListMorph >> backgroundColoringBlockOrSelector [

	^ backgroundColoringBlockOrSelector 
]

{ #category : #'background coloring' }
PluggableListMorph >> backgroundColoringBlockOrSelector: aSelector [

	backgroundColoringBlockOrSelector := aSelector
]

{ #category : #initialization }
PluggableListMorph >> basicGetListElementSelector: aSymbol [
	"specify a selector that can be used to obtain a single element in the underlying list"
	getListElementSelector := aSymbol.

]

{ #category : #'event handling' }
PluggableListMorph >> basicKeyPressed: aChar [
	"Return the index of the element matching the keystrokes.
	Returns 0 if nothing found"

	| nextSelection milliSeconds slowKeyStroke oldSelection |
	nextSelection := oldSelection := self getCurrentSelectionIndex.
	milliSeconds := Time millisecondClockValue.
	slowKeyStroke := milliSeconds - lastKeystrokeTime > 500.
	lastKeystrokeTime := milliSeconds.
	self searchedElement: nil.
	lastKeystrokes := slowKeyStroke
		ifTrue: [ aChar asLowercase asString ]
		ifFalse: [ lastKeystrokes , aChar asLowercase asString ].	"forget previous keystrokes and search in following elements"	"append quick keystrokes but don't move selection if it still matches"	"Get rid of blanks and style used in some lists"
	nextSelection := self listForSearching findFirst: [ :a | a beginsWith: lastKeystrokes fromList: self ].
	nextSelection isZero
		ifTrue: [ ^ 0 ].	"No change if model is locked"
	model okToChange
		ifFalse: [ ^ 0 ].	"No change if model is locked"	"The following line is a workaround around the behaviour of OBColumn>>selection:,
	 which deselects when called twice with the same argument."
	oldSelection = nextSelection
		ifTrue: [ ^ 0 ].	"change scrollbarvalue"
	self searchedElement: nextSelection.
	^ nextSelection
]

{ #category : #accessing }
PluggableListMorph >> basicWrapSelector: aSymbol [
	wrapSelector := aSymbol.
]

{ #category : #'multi-selection' }
PluggableListMorph >> beMultipleSelection [

	multipleSelection := true
]

{ #category : #'multi-selection' }
PluggableListMorph >> beSingleSelection [

	multipleSelection := false
]

{ #category : #geometry }
PluggableListMorph >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme listNormalBorderStyleFor: self]
		ifFalse: [self theme listDisabledBorderStyleFor: self]
]

{ #category : #'model access' }
PluggableListMorph >> changeModelSelection: anInteger [
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil:
		[model perform: setIndexSelector with: anInteger].
	self isMultipleSelection
		ifTrue: [ 
			self listSelectionAt: self lastNonZeroIndex put: false.
			self listSelectionAt: anInteger put: true ]
]

{ #category : #'model access' }
PluggableListMorph >> commandKeyTypedIntoMenu: evt [
	"The user typed a command-key into a menu which has me as its command-key handler"

	^ self modifierKeyPressed: evt
]

{ #category : #menus }
PluggableListMorph >> copyListToClipboard [
	"Copy my items to the clipboard as a multi-line string"

	| stream |
	stream := (String new: self getList size * 40) writeStream.
	list do: [:ea | stream nextPutAll: ea asString] separatedBy: [stream nextPut: Character cr].
	Clipboard clipboardText: stream contents
]

{ #category : #menus }
PluggableListMorph >> copySelectionToClipboard [
	"Copy my selected item to the clipboard as a string"

	self selection ifNotNil: [ Clipboard clipboardText: self selection asString ] ifNil: [ self flash ]
]

{ #category : #'multi-selection' }
PluggableListMorph >> defaultMultipleSelectionValue [

	^ false
]

{ #category : #'scroll cache' }
PluggableListMorph >> deriveHScrollRange [

	|  unadjustedRange totalRange |
	(list isNil or: [list isEmpty]) 
		ifTrue:[hScrollRangeCache := Array with: 0 with: 0 with: 0 with: 0 with: 0 ]
		ifFalse:[ 
			unadjustedRange := self listMorph hUnadjustedScrollRange.
			totalRange := unadjustedRange + self hExtraScrollRange + self hMargin.
			hScrollRangeCache := Array 
										with: totalRange 
										with: unadjustedRange 
										with: list size 
										with: list first 
										with: list last .
		].

]

{ #category : #selection }
PluggableListMorph >> deselectAll [

	self isMultipleSelection ifFalse: [ ^ self ].
	self resetListSelection
]

{ #category : #configuration }
PluggableListMorph >> disable [
	"Disable the receiver."
	
	self enabled: false
]

{ #category : #events }
PluggableListMorph >> doubleClick: event [
	| index |
	doubleClickSelector ifNil: [^super doubleClick: event].
	index := self rowAtLocation: event position.
	index = 0 ifTrue: [^super doubleClick: event].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector
]

{ #category : #initialization }
PluggableListMorph >> doubleClickSelector: aSymbol [
	doubleClickSelector := aSymbol
]

{ #category : #accessing }
PluggableListMorph >> dragItemSelector [
	^dragItemSelector
]

{ #category : #accessing }
PluggableListMorph >> dragItemSelector: aSymbol [
	dragItemSelector := aSymbol.
	aSymbol ifNotNil:[self dragEnabled: true].
]

{ #category : #accessing }
PluggableListMorph >> dropItemSelector [
	^dropItemSelector
]

{ #category : #accessing }
PluggableListMorph >> dropItemSelector: aSymbol [
	dropItemSelector := aSymbol.
	aSymbol ifNotNil:[self dropEnabled: true].
]

{ #category : #configuration }
PluggableListMorph >> enable [
	"Enable the receiver."
	
	self enabled: true
]

{ #category : #configuration }
PluggableListMorph >> enabled [
	"Answer the enablement state of the receiver."

	^enabled ifNil: [true]
]

{ #category : #configuration }
PluggableListMorph >> enabled: aBoolean [
	"Set the enablement state of the receiver."

	aBoolean = self enabled
		ifFalse: [ enabled :=  aBoolean.
				self adoptPaneColor: self paneColor;
					changed]
]

{ #category : #geometry }
PluggableListMorph >> extent: newExtent [
	super extent: newExtent.
	
	"Change listMorph's bounds to the new width. It is either the size
	of the widest list item, or the size of self, whatever is bigger"
	self listMorph width: ((self width max: listMorph hUnadjustedScrollRange) + 20). 

]

{ #category : #geometry }
PluggableListMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	
	^self enabled
		ifTrue: [self theme listNormalFillStyleFor: self]
		ifFalse: [self theme listDisabledFillStyleFor: self]
]

{ #category : #geometry }
PluggableListMorph >> focusBounds [
	"Answer the bounds for drawing the focus indication."

	^self theme listFocusBoundsFor: self
]

{ #category : #initialization }
PluggableListMorph >> font [

	^ self listMorph font

]

{ #category : #initialization }
PluggableListMorph >> font: aFontOrNil [
	self listMorph font: aFontOrNil.

]

{ #category : #'model access' }
PluggableListMorph >> getCurrentSelectionIndex [
	"Answer the index of the current selection."

	getIndexSelector ifNil: [^0].
	^model perform: getIndexSelector
]

{ #category : #accessing }
PluggableListMorph >> getEnabledSelector [
	"Answer the value of getEnabledSelector"

	^ getEnabledSelector ifNil: [ nil ]
]

{ #category : #accessing }
PluggableListMorph >> getEnabledSelector: aSymbol [
	"Set the value of getEnabledSelector"

	getEnabledSelector := aSymbol.
	self updateEnabled
]

{ #category : #accessing }
PluggableListMorph >> getIndexSelector: aSelector [

	getIndexSelector := aSelector
]

{ #category : #'model access' }
PluggableListMorph >> getList [
	"Answer the list to be displayed.  Caches the returned list in the 'list' ivar"
	getListSelector == nil ifTrue: [^ #()].
	list := model perform: getListSelector.
	list == nil ifTrue: [^ #()].
	list := list collectWithIndex: [ :item :index | self wrapItem: item index: index ].
	^ list
]

{ #category : #accessing }
PluggableListMorph >> getListElementSelector [

	^ getListElementSelector
]

{ #category : #initialization }
PluggableListMorph >> getListElementSelector: aSymbol [
	"specify a selector that can be used to obtain a single element in the underlying list"
	self basicGetListElementSelector: aSymbol.
	list := nil.  "this cache will not be updated if getListElementSelector has been specified, so go ahead and remove it"
	self updateList.
]

{ #category : #'model access' }
PluggableListMorph >> getListItem: index [
	"get the index-th item in the displayed list"
	| element |
	getListElementSelector ifNotNil: [ ^ self wrapItem: (model perform: getListElementSelector with: index) index: index ].
	list ifNotNil: [ ^ list at: index ].
	
	element := self getList at: index.
	^ self wrapItem: element index: index
]

{ #category : #selection }
PluggableListMorph >> getListSelector [
	^ getListSelector
]

{ #category : #initialization }
PluggableListMorph >> getListSelector: sel [
	"Set the receiver's getListSelector as indicated, and trigger a recomputation of the list"

	getListSelector := sel.
	self changed.
	self updateList.
]

{ #category : #'model access' }
PluggableListMorph >> getListSize [
	"return the current number of items in the displayed list"
	getListSizeSelector ifNotNil: [ ^model perform: getListSizeSelector ].
	^self getList size
]

{ #category : #initialization }
PluggableListMorph >> getListSizeSelector: aSymbol [
	"specify a selector that can be used to specify the list's size"
	getListSizeSelector := aSymbol
]

{ #category : #menu }
PluggableListMorph >> getMenu: shiftKeyState [
	"Answer the menu for this text view, supplying an empty menu to be filled in. If the menu selector takes an extra argument, pass in the current state of the shift key."

	| aMenu |
	aMenu := super getMenu: shiftKeyState.
	aMenu ifNotNil: [aMenu commandKeyHandler: self].
	^ aMenu
]

{ #category : #accessing }
PluggableListMorph >> getSelectionListSelector: getListSel [

	getSelectionListSelector := getListSel
]

{ #category : #scrolling }
PluggableListMorph >> hExtraScrollRange [
	"Return the amount of extra blank space to include to the right of the scroll content."
	^5 

]

{ #category : #scrolling }
PluggableListMorph >> hUnadjustedScrollRange [
	"Return the entire scrolling range."

	^self listMorph hUnadjustedScrollRange
]

{ #category : #events }
PluggableListMorph >> handleBasicKeys: aBoolean [
	"set whether the list morph should handle basic keys like arrow keys, or whether everything should be passed to the model"
	handlesBasicKeys := aBoolean
]

{ #category : #'event handling' }
PluggableListMorph >> handleFocusEvent: anEvent [
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."
	
	self processEvent: anEvent. "give submorphs a chance"
	(anEvent isMouse and: [anEvent isMouseDown and: [(self fullContainsPoint: anEvent position) not]])
		ifFalse: [^super handleFocusEvent: anEvent].
	"click outside - pass to event handler"
	self eventHandler
		ifNotNil: [self eventHandler mouseDown: anEvent fromMorph: self]
]

{ #category : #'event handling' }
PluggableListMorph >> handleMouseMove: anEvent [
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent anyButtonPressed and:[anEvent hand mouseFocus == self]) ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent.
	(self handlesMouseStillDown: anEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #handleMouseStillDown: 
			at: Time millisecondClockValue
			arguments: {anEvent copy resetHandlerFields}
			stepTime: 1].

]

{ #category : #events }
PluggableListMorph >> handlesBasicKeys [
	" if ya don't want the list to automatically handle non-modifier key 
	(excluding shift key) input, return false"
	^ handlesBasicKeys ifNil: [ true ]
]

{ #category : #'event handling' }
PluggableListMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #'event handling' }
PluggableListMorph >> handlesMouseOverDragging: evt [
	"Yes, for mouse down highlight."
	
	^true
]

{ #category : #drawing }
PluggableListMorph >> highlightSelection [
]

{ #category : #initialization }
PluggableListMorph >> initForKeystrokes [
	canMove := true.
	lastKeystrokeTime := 0.
	lastKeystrokes := ''
]

{ #category : #initialization }
PluggableListMorph >> initialize [

	super initialize.
	self initForKeystrokes.
]

{ #category : #keymapping }
PluggableListMorph >> initializeShortcuts: aKMDispatcher [
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

{ #category : #'multi-selection' }
PluggableListMorph >> isMultipleSelection [

	^ self multipleSelection
]

{ #category : #'multi-selection' }
PluggableListMorph >> isSingleSelection [

	^ self multipleSelection not
]

{ #category : #'model access' }
PluggableListMorph >> itemSelectedAmongMultiple: index [
	"return whether the index-th row is selected.  Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany"

	^ self isMultipleSelection
		ifTrue: [ (self listSelectionAt: index) == true ]
		ifFalse: [ false ]
]

{ #category : #'event handling' }
PluggableListMorph >> keyStroke: event [ 
	"Process keys 
	specialKeys are things like up, down, etc. ALWAYS HANDLED 
	modifierKeys are regular characters either 1) accompanied with ctrl, 
	cmd or 2) any character if the list doesn't want to handle basic 
	keys (handlesBasicKeys returns false) 
	basicKeys are any characters"
	| aChar |
	
	(self scrollByKeyboard: event) ifTrue: [^self].
	(self navigationKey: event) ifTrue: [^self].
	aChar := event keyCharacter.

	keystrokeSelector ifNotNil: [ 
		(self keystrokeAction: event) ifTrue: [ ^ self ] ].
	
	aChar asciiValue < 32 ifTrue: [^ self specialKeyPressed: event ].
	(event anyModifierKeyPressed or: [self handlesBasicKeys not])
		ifTrue: [^ self modifierKeyPressed: event].
	^ self basicKeyPressed: aChar
]

{ #category : #'event handling' }
PluggableListMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #'model access' }
PluggableListMorph >> keystrokeAction: event [ 
	| returnValue |
	
	keystrokeSelector ifNil: [ ^ nil ].

	returnValue := model 
						perform: keystrokeSelector
						withEnoughArguments: { event. self }.
	
	^ returnValue = true
]

{ #category : #initialization }
PluggableListMorph >> keystrokeActionSelector: keyActionSel [
	"Set the keystroke action selector as specified"

	keystrokeActionSelector := keyActionSel
]

{ #category : #'model access' }
PluggableListMorph >> keystrokeSelector: aSymbol [
	keystrokeSelector := aSymbol
]

{ #category : #accessing }
PluggableListMorph >> lastNonZeroIndex [

	^ lastNonZeroIndex ifNil: [ lastNonZeroIndex := 0 ]
]

{ #category : #searching }
PluggableListMorph >> listForSearching [

	^ getListSelector
		ifNotNil: [ self getList ]
		ifNil: [ getListElementSelector
				ifNil: [ #() ]
				ifNotNil: [ 
					(1 to: self getListSize) collect: [:index | self getListItem: index ]]]
			
]

{ #category : #initialization }
PluggableListMorph >> listItemHeight [
	"This should be cleaned up.  The list should get spaced by this parameter."
	^ 12
]

{ #category : #accessing }
PluggableListMorph >> listMorph [
	listMorph ifNil: [
		"crate this lazily, in case the morph is legacy"
		listMorph := self listMorphClass new.
		listMorph listSource: self.
		listMorph width: self scroller width.
		listMorph color: self textColor ].

	listMorph owner ~~ self scroller ifTrue: [
		"list morph needs to be installed.  Again, it's done this way to accomodate legacy PluggableListMorphs"
		self scroller removeAllMorphs.
		self scroller addMorph: listMorph ].

	^listMorph
]

{ #category : #initialization }
PluggableListMorph >> listMorphClass [
	^LazyListMorph
]

{ #category : #'multi-selection' }
PluggableListMorph >> listSelectionAt: index [

	index isZero ifFalse:[ lastNonZeroIndex := index ].
	getSelectionListSelector ifNil:[^false].
	^model perform: getSelectionListSelector with: index
]

{ #category : #'multi-selection' }
PluggableListMorph >> listSelectionAt: index put: value [

	self searchedElement: nil.
	setSelectionListSelector ifNil:[^false].
	^model perform: setSelectionListSelector with: index with: value
]

{ #category : #selection }
PluggableListMorph >> maximumSelection [
	^ self getListSize
]

{ #category : #selection }
PluggableListMorph >> minimumSelection [
	^ 1
]

{ #category : #'model access' }
PluggableListMorph >> modifierKeyPressed: event [ 
	| args aChar |
	aChar := event keyCharacter.
	keystrokeActionSelector ifNil: [ ^ nil ].
	args := keystrokeActionSelector numArgs.
	args = 1 ifTrue: [^ model perform: keystrokeActionSelector with: aChar].
	args = 2 
		ifTrue: 
			[^model 
				perform: keystrokeActionSelector
				with: aChar
				with: self].
	^self error: 'keystrokeActionSelector must be a 1- or 2-keyword symbol'
]

{ #category : #'event handling' }
PluggableListMorph >> mouseDown: evt [
	"Changed to only take focus if wanted."

	| selectors row |
	row := self rowAtLocation: evt position.
	evt yellowButtonPressed
		ifTrue: [ 
			self isMultipleSelection
				ifTrue: [ 
					evt commandKeyPressed
						ifFalse: [ 
							"right click"
							(self yellowButtonActivity: evt shiftPressed)
								ifTrue: [ ^ super mouseDown: evt ] ] ]
				ifFalse: [ 
					(self yellowButtonActivity: evt shiftPressed)
						ifTrue: [ ^ super mouseDown: evt ] ] ].	"First check for option (menu) click"
	self enabled
		ifFalse: [ ^ super mouseDown: evt ].
	self wantsKeyboardFocus
		ifTrue: [ self takeKeyboardFocus ].
	row := self rowAtLocation: evt position.
	row = 0
		ifTrue: [ ^ super mouseDown: evt ].
	self mouseDownRow: row.
	self isMultipleSelection
		ifTrue: [ self mouseDownOnMultiple: evt forRow: row ].
	selectors := Array
		with: #click:
		with: (doubleClickSelector ifNotNil: [ #doubleClick: ])
		with: nil
		with:
			(self dragEnabled
				ifTrue: [ #startDrag: ]
				ifFalse: [ nil ]).
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10.	"pixels"
	super mouseDown: evt
]

{ #category : #'multi-selection' }
PluggableListMorph >> mouseDownOnMultiple: event forRow: row [

	| anInteger oldIndex oldVal valueKeeper |
	
	"Set meaning for subsequent dragging of selection"
	canMove ifFalse: [ ^ self ].
	canMove := false.
	model okToChange ifFalse: [ 
		canMove := true.
		^ self ].
	canMove := true.
	
	dragOnOrOff := (self listSelectionAt: row) not.
	valueKeeper := dragOnOrOff.
	" I store the value because #mouseUpOnMultiple: can reset dragOnOrOff before the end of this method (in case of halt by example)"
	(event shiftPressed not and: [event yellowButtonPressed not and: [ self autoDeselect ]])
		ifTrue: [ self resetListSelection ].
	oldIndex := self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [oldVal := self listSelectionAt: oldIndex].

	"Set or clear new primary selection (listIndex)"
	anInteger := valueKeeper
					ifTrue: [ row ]
					ifFalse: [ 0 ].
	
	setIndexSelector ifNotNil:
		[ model perform: setIndexSelector with: anInteger ].
	
	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
	
	event shiftPressed
		ifTrue: [((oldIndex max: 1) min: row) to: (oldIndex max: row) do: [:i |
					self listSelectionAt: i put: valueKeeper].
				self changed]
		ifFalse: [self listSelectionAt: row put: valueKeeper].
]

{ #category : #accessing }
PluggableListMorph >> mouseDownRow [
	"Answer the mouse down row or nil if none."

	^self listMorph mouseDownRow
]

{ #category : #events }
PluggableListMorph >> mouseDownRow: anIntegerOrNil [
	"Set the mouse down row or nil if none."

	self listMorph mouseDownRow: anIntegerOrNil
]

{ #category : #'event handling' }
PluggableListMorph >> mouseEnter: event [
	"Changed to take keyboardFocusOnMouseDown  into account."
	
	super mouseEnter: event.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown
		ifFalse: [self takeKeyboardFocus]
]

{ #category : #'event handling' }
PluggableListMorph >> mouseEnterDragging: evt [
	"The mouse has entered with a button down.
	Workaround for apparent flaw in MouseOverHandler constantly
	sending this message when dragging.
	Do nothing if disabled."
	
	|row oldPDR|
	self enabled ifFalse: [^self].
	row := self rowAtLocation: evt position.
	(self dragEnabled or: [evt hand hasSubmorphs]) ifFalse: [
		row = 0 ifTrue: [ ^self ].
		self listMorph mouseDownRow: row].
	(evt hand hasSubmorphs and:[self dropEnabled]) ifFalse: ["no d&d"
		^super mouseEnterDragging: evt].
	potentialDropRow = row ifTrue: [^self].
	oldPDR := potentialDropRow.
	potentialDropRow := row.
	evt hand newMouseFocus: self.
	"above is ugly but necessary for now"
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt )
		ifTrue: [self changed]
		ifFalse: [(oldPDR ifNil: [0]) > 0
				ifTrue: [self resetPotentialDropRow]
				ifFalse: [potentialDropRow := 0]]
]

{ #category : #'event handling' }
PluggableListMorph >> mouseLeaveDragging: anEvent [
	"The mouse has left with a button down."
	
	(self dragEnabled or: [anEvent hand hasSubmorphs]) ifFalse: [
		self listMorph mouseDownRow: nil].
	(self dropEnabled and: [anEvent hand hasSubmorphs]) ifFalse: ["no d&d"
		^super mouseLeaveDragging: anEvent].
	self resetPotentialDropRow.
	anEvent hand releaseMouseFocus: self.
	"above is ugly but necessary for now"
]

{ #category : #'event handling' }
PluggableListMorph >> mouseMove: evt [

	self isMultipleSelection
		ifTrue: [ self mouseMoveOnMultiple: evt ]
		ifFalse: [ self mouseMoveOnSingle:evt ]
]

{ #category : #'multi-selection' }
PluggableListMorph >> mouseMoveOnMultiple: event [

	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"
	| oldIndex oldVal row |
	canMove ifFalse: [ ^ self ].
	
	event position y < self top 
		ifTrue: 
			[ scrollBar scrollUp: 1.
			row := self rowAtLocation: scroller topLeft + (1 @ 1)]
		ifFalse: 
			[ row := event position y > self bottom 
				ifTrue: 
					[scrollBar scrollDown: 1.
					self rowAtLocation: scroller bottomLeft + (1 @ -1)]
				ifFalse: [ self rowAtLocation: event position ]].
	
	row = 0 ifTrue: [ ^ super mouseDown: event ].

	(self potentialDropItem notNil and: [ self dropEnabled ]) ifTrue: [ ^ self ].
	
	dragOnOrOff ifNil: 
			["Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
				dragOnOrOff := (self listSelectionAt: row) not ].

	"Set meaning for subsequent dragging of selection"
	oldIndex := self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal := self listSelectionAt: oldIndex ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	self listSelectionAt: row put: dragOnOrOff.
	row changed
]

{ #category : #'multi-selection' }
PluggableListMorph >> mouseMoveOnSingle: evt [
	"The mouse has moved with a button down.
	Do nothing if disabled."

	|row|
	self enabled ifFalse: [^self].
	row := self rowAtLocation: evt position.
	evt hand hasSubmorphs ifFalse: [
		((self containsPoint: evt position) and: [ row ~= 0 ]) 
			ifTrue: [self mouseDownRow: row]
			ifFalse: [self mouseDownRow: nil]].
	(self dropEnabled and:[evt hand hasSubmorphs]) 
		ifFalse: [^self eventHandler ifNotNil:
				[self eventHandler mouseMove: evt fromMorph: self]].
	(self containsPoint: evt position) 
		ifTrue: [self mouseEnterDragging: evt]
		ifFalse: [self mouseLeaveDragging: evt]
]

{ #category : #'event handling' }
PluggableListMorph >> mouseUp: evt [

	self isMultipleSelection
		ifTrue: [ self mouseUpOnMultiple: evt ]
		ifFalse: [ self mouseUpOnSingle: evt ]
]

{ #category : #'multi-selection' }
PluggableListMorph >> mouseUpOnMultiple: event [
	"Reset the mouseDownRow."
	
	dragOnOrOff := nil.  "So improperly started drags will have not effect".
	event hand hasSubmorphs ifFalse: [
		self mouseDownRow: nil]
]

{ #category : #'multi-selection' }
PluggableListMorph >> mouseUpOnSingle: event [
	"The mouse came up within the list; take appropriate action"
	
	| row mdr |
	row := self rowAtLocation: event position.
	event hand hasSubmorphs ifFalse: [
		mdr := self mouseDownRow.
		 self mouseDownRow: nil.
		mdr ifNil: [^self]].

	(self enabled and: [model okToChange])
		ifFalse: [^ self].

	"No change if model is locked or receiver disabled"
	row == self selectionIndex
		ifTrue: [
			self autoDeselect 
				ifTrue: [ row = 0 ifFalse: [ self changeModelSelection: 0 ] ]
				ifFalse: [ self changeModelSelection: row ] ]
		ifFalse: [ self changeModelSelection: row ].
	Cursor normal show
]

{ #category : #'multi-selection' }
PluggableListMorph >> multipleSelection [

	^ multipleSelection ifNil: [ multipleSelection := self defaultMultipleSelectionValue ]
]

{ #category : #events }
PluggableListMorph >> navigationKey: anEvent [

	self isMultipleSelection
		ifTrue: [ | keyString |
			keyString := anEvent keyString.
			keyString = '<Cmd-a>'
				ifTrue: [ 
					self selectAll.
					^ true ].
			keyString = '<Cmd-A>'
				ifTrue: [ 
					self deselectAll.
					^ true ]	].
				
	^ super navigationKey: anEvent
]

{ #category : #scrolling }
PluggableListMorph >> numSelectionsInView [
	"Answer the scroller's height based on the average number of submorphs."
		
	"ugly hack, due to code smell.
	PluggableListMorph added another level of indirection, 
	There is always only one submorph - a LazyListMorph which holds the actual list,
	but TransformMorph doesn't know that and we are left with a breach of interface."
		
	^scroller numberOfItemsPotentiallyInViewWith: scroller submorphs last getListSize.
]

{ #category : #'multi-selection' }
PluggableListMorph >> on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel keystroke: keyActionSel [
	"setup a whole load of pluggability options"
	getSelectionListSelector := getListSel.
	setSelectionListSelector := setListSel.
	self on: anObject list: listSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel.
	self beMultipleSelection.
	^ self

]

{ #category : #initialization }
PluggableListMorph >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [ 
	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	self autoDeselect: true.
	self borderWidth: 1.
	self updateList.
	self selectionIndex: self getCurrentSelectionIndex.
	self initForKeystrokes
]

{ #category : #geometry }
PluggableListMorph >> optimalExtent [
	"Answer the extent of the list morph."

	^self listMorph extent + (self borderWidth * 2) + self scrollBarThickness
]

{ #category : #'drag and drop' }
PluggableListMorph >> potentialDropItem [
	"return the item that the most recent drop hovered over, or nil if there is no potential drop target"
	self potentialDropRow = 0 ifTrue: [ ^self ].
	^self getListItem: self potentialDropRow
]

{ #category : #'drag and drop' }
PluggableListMorph >> potentialDropRow [
	"return the row of the item that the most recent drop hovered over, or 0 if there is no potential drop target"
	^potentialDropRow ifNil: [ 0 ].

]

{ #category : #'scroll cache' }
PluggableListMorph >> resetHScrollRange [

	hScrollRangeCache := nil.
	self deriveHScrollRange.

]

{ #category : #'scroll cache' }
PluggableListMorph >> resetHScrollRangeIfNecessary [

	hScrollRangeCache ifNil: [ ^self deriveHScrollRange ].

	(list isNil or: [list isEmpty]) 
		ifTrue:[^hScrollRangeCache := Array with: 0 with: 0 with: 0 with: 0 with: 0].

"Make a guess as to whether the scroll ranges need updating based on whether the size, first item, or last item of the list has changed"
	(
		(hScrollRangeCache third == list size) and: [
		(hScrollRangeCache fourth == list first) and: [
		(hScrollRangeCache fifth == list last)
	]])
		ifFalse:[self deriveHScrollRange].


]

{ #category : #selection }
PluggableListMorph >> resetListSelection [

	self resetListSelectionSilently.
	self changed
]

{ #category : #selection }
PluggableListMorph >> resetListSelectionSilently [

	self resetListSelector ifNotNil: [:sel | self model perform: sel ]
]

{ #category : #'multi-selection' }
PluggableListMorph >> resetListSelector [

	^ resetListSelector
]

{ #category : #'multi-selection' }
PluggableListMorph >> resetListSelector: aSelector [

	resetListSelector := aSelector
]

{ #category : #'drag and drop' }
PluggableListMorph >> resetPotentialDropRow [
	potentialDropRow ifNotNil: [
	potentialDropRow ~= 0 ifTrue: [
		potentialDropRow := 0.
		self changed. ] ]
]

{ #category : #scrolling }
PluggableListMorph >> resizeScrollBars [
	"Fixed to not use deferred message that incorrectly
	sets scroll deltas/interval."

	(self extent = self defaultExtent)
		ifFalse: [super resizeScrollBars]
]

{ #category : #accessing }
PluggableListMorph >> rowAtLocation: aPoint [
	"Return the row at the given point or 0 if outside"
	| pointInListMorphCoords |
	pointInListMorphCoords := (self scroller transformFrom: self) transform: aPoint.
	^self listMorph rowAtLocation: pointInListMorphCoords.
]

{ #category : #geometry }
PluggableListMorph >> scrollDeltaHeight [
	"Return the increment in pixels which this pane should be scrolled."
	^ self font height
]

{ #category : #geometry }
PluggableListMorph >> scrollDeltaWidth [
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2


]

{ #category : #selection }
PluggableListMorph >> scrollSelectionIntoView [
	"make sure that the current selection is visible"
	| row |
	row := self getCurrentSelectionIndex.
	self scrollSelectionToRow: row
]

{ #category : #selection }
PluggableListMorph >> scrollSelectionToRow: row [
	"make sure that the current selection is visible"

	row = 0 ifTrue: [ ^ self ].
	self scrollToShow: (self listMorph drawBoundsForRow: row)
]

{ #category : #searching }
PluggableListMorph >> searchedElement [

	^ searchedElement
]

{ #category : #searching }
PluggableListMorph >> searchedElement: anInteger [

	searchedElement := anInteger.
	anInteger
		ifNil: [
			" just for redrawn "
			self vScrollValue: (self scrollValue y) ]
		ifNotNil: [ self vScrollValue: ((anInteger-1)/self getListSize) ]
]

{ #category : #searching }
PluggableListMorph >> secondarySelectionColor [
	^ self theme secondarySelectionColor
]

{ #category : #selection }
PluggableListMorph >> selectAll [

	self isMultipleSelection ifFalse: [ ^ self ].
	1 to: self maximumSelection do: [: i | self listSelectionAt: i put: true ]
]

{ #category : #searching }
PluggableListMorph >> selectSearchedElement [

	self searchedElement ifNotNil: [: index |	
			 ActiveEvent commandKeyPressed	ifFalse: [ self resetListSelectionSilently ].
			self changeModelSelection: index.
			self isMultipleSelection
				ifTrue: [ self listSelectionAt: index put: true ].
			self vScrollValue: ((index-1)/self getListSize) ]
]

{ #category : #selection }
PluggableListMorph >> selectedMorph [
	"this doesn't work with the LargeLists patch!  Use #selectionIndex and #selection instead."
	^self scroller submorphs at: self selectionIndex
]

{ #category : #selection }
PluggableListMorph >> selection [ 
	self selectionIndex = 0 ifTrue: [ ^nil ].
	list ifNotNil: [ ^list at: self selectionIndex ].
	^ self getListItem: self selectionIndex
]

{ #category : #selection }
PluggableListMorph >> selection: item [
	"Called from outside to request setting a new selection."

	self selectionIndex: (self getList indexOf: item)
]

{ #category : #accessing }
PluggableListMorph >> selectionColor [
	"Answer the colour to use for selected items."

	^ selectionColor ifNil: [self theme selectionColor] 
]

{ #category : #accessing }
PluggableListMorph >> selectionColor: aColor [
	"Set the colour for selected items."

	| window |
	selectionColor := aColor.
	window := self ownerThatIsA: SystemWindow.

	self selectionColorToUse: ((self theme fadedBackgroundWindows not or: [ window isNil or: [ window isActive ] ])
		ifTrue: [ aColor ]
		ifFalse: [ self theme unfocusedSelectionColor ])
]

{ #category : #accessing }
PluggableListMorph >> selectionColorToUse [
	"Answer the colour to use for selected items."

	^ selectionColorToUse ifNil: [ self theme selectionColor ]
]

{ #category : #accessing }
PluggableListMorph >> selectionColorToUse: aColor [
	"Set the colour for selected items."

	aColor = self selectionColorToUse ifTrue: [^self].
	selectionColorToUse := aColor.
	self listMorph selectionFrameChanged
]

{ #category : #selection }
PluggableListMorph >> selectionIndex [
	"return the index we have currently selected, or 0 if none"
	^self listMorph selectedRow ifNil: [ 0 ]
]

{ #category : #selection }
PluggableListMorph >> selectionIndex: index [
	"Called internally to select the index-th item."
	| row |
	
	self unhighlightSelection.
	row := index ifNil: [ 0 ].
	row := row min: self maximumSelection.  "make sure we don't select past the end"
	self listMorph selectedRow: row.
	self highlightSelection.
	self scrollSelectionToRow: row.
]

{ #category : #separator }
PluggableListMorph >> separatorAfterARow: aRow [

	
	
	aRow ifNil: [ ^ false ].

	self separatorBlockOrSelector
		ifNotNil: [:blockOrSelector || anItem |
			anItem := getListElementSelector
						ifNil: [ list at: aRow ifAbsent: [ ^ false ]]
						ifNotNil: [ model perform: getListElementSelector with: aRow ].
						
			^ blockOrSelector isBlock
				ifTrue: [ blockOrSelector cull: anItem cull: aRow ]
				ifFalse: [
					blockOrSelector isSymbol
						ifTrue: [ blockOrSelector numArgs == 0
									ifTrue: [ anItem perform: blockOrSelector ]
									ifFalse: [ self model perform: blockOrSelector withEnoughArguments: { anItem. aRow} ]]
						ifFalse: [ false ]]].
	
	^ false
]

{ #category : #separator }
PluggableListMorph >> separatorBlockOrSelector [

	^ separatorBlockOrSelector
]

{ #category : #separator }
PluggableListMorph >> separatorBlockOrSelector: aBlockOrSelector [

	separatorBlockOrSelector := aBlockOrSelector
]

{ #category : #separator }
PluggableListMorph >> separatorColor [

	^ separatorColor ifNil: [ separatorColor := Color gray ]
]

{ #category : #separator }
PluggableListMorph >> separatorColor: aColor [

	separatorColor := aColor
]

{ #category : #separator }
PluggableListMorph >> separatorSize [

	^ separatorSize ifNil: [ separatorSize := 1 ]
]

{ #category : #separator }
PluggableListMorph >> separatorSize: anInteger [

	separatorSize := anInteger
]

{ #category : #accessing }
PluggableListMorph >> setIndexSelector: aSelector [

	setIndexSelector := aSelector
]

{ #category : #accessing }
PluggableListMorph >> setMultipleSelection: aBoolean [

	multipleSelection := aBoolean
]

{ #category : #selection }
PluggableListMorph >> setSelectedMorph: aMorph [
	self changeModelSelection: (scroller submorphs indexOf: aMorph)
]

{ #category : #accessing }
PluggableListMorph >> setSelectionListSelector: getListSel [

	setSelectionListSelector := getListSel
]

{ #category : #events }
PluggableListMorph >> specialKeyPressed: anEvent [
	"A special key with the given ascii-value was pressed; dispatch it"
	| keyString max nextSelection oldSelection howManyItemsShowing |
	
	keyString := anEvent keyString.
	keyString = '<escape>'
		ifTrue: [" escape key"
			^ ActiveEvent shiftPressed
				ifTrue:
					[self currentWorld invokeWorldMenuFromEscapeKey]
				ifFalse:[ 
					(self yellowButtonActivity: false) 
						ifTrue: [ ^ self ]]].

	keyString = '<cr>'
		ifTrue: [ 
			"enter pressed" 
			self selectSearchedElement ].

	max := self maximumSelection.
	max > 0 ifFalse: [^ self].
	nextSelection := oldSelection := self getCurrentSelectionIndex.
	
	keyString = '<down>'
		ifTrue: [" down arrow"
			self resetListSelectionSilently.
			nextSelection := oldSelection + 1.
			nextSelection > max ifTrue: [nextSelection := max]].
	
	keyString = '<up>'
		ifTrue: [ " up arrow"
			self resetListSelectionSilently.
			nextSelection := oldSelection - 1.
			nextSelection < 1 ifTrue: [nextSelection := 1]].
	keyString = '<home>'
		ifTrue: [" home"
			self resetListSelectionSilently.
			nextSelection := 1].
		
	keyString = '<end>' 
		ifTrue: [" end"
			self resetListSelectionSilently.
			nextSelection := max].
	
	howManyItemsShowing := self numSelectionsInView.
	
	keyString = '<pageUp>'
		ifTrue: 	[" page up"
			self resetListSelectionSilently.
			nextSelection := 1 max: oldSelection - howManyItemsShowing].
	keyString = '<pageDown>'
		ifTrue: [" page down"
			self resetListSelectionSilently.
			nextSelection := oldSelection + howManyItemsShowing min: max].

	(self enabled and: [model okToChange]) ifFalse: [^ self].
	
	"No change if model is locked"
	
	oldSelection = nextSelection ifTrue: [^ self].
	^ self changeModelSelection: nextSelection
]

{ #category : #'drag and drop' }
PluggableListMorph >> startDrag: evt [ 
	| transferMorph draggedItem passenger |

	dragItemSelector ifNotNil: [ ^self startDragExtended: evt ].
	evt hand hasSubmorphs ifTrue: [^ self]. 
	self dragEnabled ifFalse: [^ self].
	"Here I ensure at least one element is selected "
	ActiveHand anyButtonPressed ifFalse: [ ^self ].
			
	draggedItem := self getListItem: (self mouseDownRow ifNil: [ self lastNonZeroIndex ]).
	draggedItem ifNil: [ ^ self ].
	passenger := self model dragPassengersFor: draggedItem inMorph: self.
	passenger ifNil: [ ^ self ].
		
	transferMorph := self model transferFor: passenger from: self.
	transferMorph align: transferMorph draggedMorph bottomLeft  with: evt position.
	
	self mouseDownRow: nil.
	transferMorph dragTransferType: (self model dragTransferTypeForMorph: self).
	
	[evt hand grabMorph: transferMorph ] 
		ensure: [
			Cursor normal show.
			evt hand releaseMouseFocus: self] 
]

{ #category : #'drag and drop' }
PluggableListMorph >> startDragExtended: evt [
	"This method was defined in PluggableListMorphPlus (a subclass that got merged)" 
	
	dragItemSelector ifNil: [^self].
	evt hand hasSubmorphs ifTrue: [^ self].
	[ | dragIndex draggedItem ddm ddRect |
	(self dragEnabled and: [model okToChange]) ifFalse: [^ self].
	dragIndex := self rowAtLocation: evt position.
	dragIndex = 0 ifTrue: [^self].
	draggedItem := model perform: dragItemSelector with: dragIndex.
	draggedItem ifNil: [^self].
	self mouseDownRow: nil.
	ddm := self model transferFor: draggedItem from: self.
	ddRect := ddm draggedMorph bounds.
	ddm position: evt position - (ddRect center - ddRect origin).
	ddm dragTransferType: #dragTransfer.
	evt hand grabMorph: ddm]
		ensure: [Cursor normal show.
			evt hand newMouseFocus: self]
]

{ #category : #'event handling' }
PluggableListMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #initialization }
PluggableListMorph >> textColor [
	"Answer my default text color."
	^ self 
		valueOfProperty: #textColor 
		ifAbsent: [ self theme textColor ]

]

{ #category : #initialization }
PluggableListMorph >> textColor: aColor [
	"Set my default text color."
	self setProperty: #textColor toValue: aColor.
	self listMorph color: aColor.
]

{ #category : #initialization }
PluggableListMorph >> textHighlightColor [
	"Answer my default text highlight color."
	^self valueOfProperty: #textHighlightColor ifAbsent: [ Color red ].

]

{ #category : #initialization }
PluggableListMorph >> textHighlightColor: aColor [
	"Set my default text highlight color."
	self setProperty: #textHighlightColor toValue: aColor.

]

{ #category : #updating }
PluggableListMorph >> themeChanged [

	self setProperty: #textColor toValue: self theme textColor; updateList.
	
	self selectionColor ifNotNil: [
		self selectionColor: self theme selectionColor
	].

	super themeChanged 
]

{ #category : #drawing }
PluggableListMorph >> unhighlightSelection [

	self searchedElement: nil.
]

{ #category : #updating }
PluggableListMorph >> update: aParameter [ 
	"Refer to the comment in View|update:."

	(aParameter == getListSelector or: [ aParameter == getListElementSelector ]) ifTrue: 
		[self updateList.
		^ self].
	aParameter == getIndexSelector ifTrue:
		[self selectionIndex: self getCurrentSelectionIndex.
		^ self].
	aParameter == #allSelections ifTrue:
		[self selectionIndex: self getCurrentSelectionIndex.
		^ self changed].
	
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #setMultipleSelection: ifTrue: [ self setMultipleSelection: aParameter second ]
]

{ #category : #updating }
PluggableListMorph >> updateEnabled [
	"Update the enablement state."

	self model ifNotNil: [
		self getEnabledSelector ifNotNil: [
			self enabled: (self model perform: self getEnabledSelector)]]
]

{ #category : #updating }
PluggableListMorph >> updateList [
	| index |
	"the list has changed -- update from the model"
	self listMorph listChanged.
	self setScrollDeltas.
	scrollBar setValue: 0.0.
	index := self getCurrentSelectionIndex.
	self resetPotentialDropRow.
	index ifNotNil: [ 
		index := index min: self getListSize.
		index > 0 ifTrue: [	self selectionIndex: index].
	].
	
	self searchedElement: nil.

]

{ #category : #'debug and other' }
PluggableListMorph >> userString [
	^list ifNotNil: [ 
		String streamContents: [:strm |
			list do: [:i |
				strm nextPutAll: (i string); cr
			]
		]
	]
]

{ #category : #scrolling }
PluggableListMorph >> vExtraScrollRange [
	"Return the amount of extra blank space to include below the bottom of the scroll content."

	^8

]

{ #category : #scrolling }
PluggableListMorph >> vUnadjustedScrollRange [
	"Return the height extent of the receiver's submorphs."
	(scroller submorphs size > 0) ifFalse:[ ^0 ].
	^(scroller submorphs last fullBounds bottom)

]

{ #category : #updating }
PluggableListMorph >> verifyContents [
	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes.  Also called periodically in morphic if the smartUpdating setting is true"
	| newList existingSelection oldList |
	oldList := list ifNil: [ #() ].
	newList := self getList.
	((oldList == newList) "fastest" or: [oldList = newList]) ifTrue: [^ self].
	existingSelection := oldList isEmpty
		ifTrue: [self listMorph selectedRow]
		ifFalse: [(self selectionIndex between: 1 and: newList size)
					ifTrue: [self selectionIndex]
					ifFalse: [nil]].
	self updateList.
	existingSelection notNil
		ifTrue: [	self selectionIndex: existingSelection]
		ifFalse:	[self changeModelSelection: 0]
]

{ #category : #accessing }
PluggableListMorph >> wantsDropSelector [
	^wantsDropSelector
]

{ #category : #accessing }
PluggableListMorph >> wantsDropSelector: aSymbol [
	wantsDropSelector := aSymbol
]

{ #category : #'drag and drop' }
PluggableListMorph >> wantsDroppedMorph: aMorph event: anEvent [
	^ aMorph dragTransferType == #dragTransfer 
		ifTrue: [ dropItemSelector ifNil: [^false].
			wantsDropSelector ifNil: [^true].
			(model perform: wantsDropSelector with: aMorph passenger)]
		ifFalse: [ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self]
]

{ #category : #wrapping }
PluggableListMorph >> wrapItem: anItem index: anIndex [
	"Use the wrapSelector to get the text or string representation of a list item."
	
	^ self wrapSelector 
		ifNil: [ anItem asStringOrText]
		ifNotNil: [:selector | 
			selector numArgs == 0
				ifTrue: [ anItem perform: selector ]
				ifFalse: [ self model perform: selector withEnoughArguments: { anItem. anIndex } ]].
]

{ #category : #accessing }
PluggableListMorph >> wrapSelector [

	^ wrapSelector
]

{ #category : #accessing }
PluggableListMorph >> wrapSelector: aSymbol [

	self basicWrapSelector: aSymbol.
	self updateList.
]
