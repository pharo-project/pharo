"
Display a hierarchical list of items.  Each item should be wrapped with a ListItemWrapper.

For a simple example, look at submorphsExample.  For beefier examples, look at ObjectExplorer or FileList.
"
Class {
	#name : #SimpleHierarchicalListMorph,
	#superclass : #ScrollPane,
	#instVars : [
		'selectedMorph',
		'getListSelector',
		'keystrokeActionSelector',
		'autoDeselect',
		'columns',
		'sortingSelector',
		'getSelectionSelector',
		'setSelectionSelector',
		'potentialDropMorph',
		'lineColor',
		'lastSelection',
		'lastKeystrokeTime',
		'lastKeystrokes',
		'searchedElement'
	],
	#category : #'Morphic-Widgets-List'
}

{ #category : #shortcuts }
SimpleHierarchicalListMorph class >> buildSimpleHierarchicalListShortcuts: aBuilder [
	<keymap>
	(aBuilder shortcut: #cursorUp)
		category: #SimpleHierarchicalList
		default: Character arrowUp asKeyCombination
		do: [ :target | target setSelectionIndex: (target getSelectionIndex - 1 max: 1) ].
	(aBuilder shortcut: #cursorDown)
		category: #SimpleHierarchicalList
		default: Character arrowDown asKeyCombination
		do: [ :target | target setSelectionIndex: target getSelectionIndex + 1 ].
	(aBuilder shortcut: #cursorHome)
		category: #SimpleHierarchicalList
		default: Character home asKeyCombination
		do: [ :target | target setSelectionIndex: 1 ].
	(aBuilder shortcut: #cursorEnd)
		category: #SimpleHierarchicalList
		default: Character end asKeyCombination
		do: [ :target | target setSelectionIndex: target scroller submorphs size ].
	(aBuilder shortcut: #cursorPageUp)
		category: #SimpleHierarchicalList
		default: Character pageUp asKeyCombination
		do: [ :target | target setSelectionIndex: (target getSelectionIndex - target numSelectionsInView max: 1) ].
	(aBuilder shortcut: #cursorPageDown)
		category: #SimpleHierarchicalList
		default: Character pageDown asKeyCombination
		do: [ :target | target setSelectionIndex: target getSelectionIndex + target numSelectionsInView ].
	(aBuilder shortcut: #cursorRight)
		category: #SimpleHierarchicalList
		default: Character arrowRight asKeyCombination
		do: [ :target | 
			target selectedMorph
				ifNotNil: [ 
					target selectedMorph canExpand
						ifTrue: [ 
							target selectedMorph isExpanded not
								ifTrue: [ target toggleExpandedState: target selectedMorph ]
								ifFalse: [ target setSelectionIndex: target getSelectionIndex + 1 ] ] ] ].
	(aBuilder shortcut: #cursorLeft)
		category: #SimpleHierarchicalList
		default: Character arrowLeft asKeyCombination
		do: [ :target | 
			| selectedMorph |
			(selectedMorph := target selectedMorph)
				ifNotNil: [ 
					selectedMorph isExpanded
						ifTrue: [ target toggleExpandedState: selectedMorph ]
						ifFalse: [ 
							| i |
							"Search parent."
							selectedMorph indentLevel > 0
								ifTrue: [ 
									i := target getSelectionIndex max: 1.
									[ i > 1 and: [ (target scroller submorphs at: i) indentLevel >= selectedMorph indentLevel ] ]
										whileTrue: [ i := i - 1 max: 1 ].
									target setSelectionIndex: i ] ] ] ]
]

{ #category : #examples }
SimpleHierarchicalListMorph class >> exampleSubmorphs [
	"display a hierarchical list of the World plus its submorphs plus its submorphs' submorphs etc."
	"[SimpleHierarchicalListMorph exampleSubmorphs]"
	| morph |
	morph :=
		SimpleHierarchicalListMorph
			on: [ Array with:  (MorphWithSubmorphsWrapper with: self currentWorld)  ]
			list: #value
			selected: nil
			changeSelected: nil
			menu: nil
			keystroke: nil.

	morph openInWindow
]

{ #category : #'instance creation' }
SimpleHierarchicalListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel [
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
SimpleHierarchicalListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel [
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
SimpleHierarchicalListMorph class >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel

]

{ #category : #'dropping/grabbing' }
SimpleHierarchicalListMorph >> acceptDroppingMorph: aMorph event: evt [

	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.
	self resetPotentialDropMorph.
	evt hand releaseMouseFocus: self.
	Cursor normal show.

]

{ #category : #private }
SimpleHierarchicalListMorph >> addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent [

	| priorMorph newCollection firstAddition |
	priorMorph := nil.
	newCollection := (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		(aCollection asSortedCollection: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]) asOrderedCollection
	] ifFalse: [
		aCollection
	].
	firstAddition := nil.
	newCollection do: [:item | 
		priorMorph := self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: newIndent.
		firstAddition ifNil: [firstAddition := priorMorph].
		morphList add: priorMorph.
		((item hasEquivalentIn: expandedItems) or: [priorMorph isExpanded]) ifTrue: [
			priorMorph isExpanded: true.
			priorMorph 
				addChildrenForList: self 
				addingTo: morphList
				withExpandedItems: expandedItems.
		].
	].
	^firstAddition
	

]

{ #category : #private }
SimpleHierarchicalListMorph >> addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean [

	| priorMorph morphList newCollection |
	priorMorph := nil.
	newCollection := (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		(aCollection asSortedCollection: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]) asOrderedCollection
	] ifFalse: [
		aCollection
	].
	morphList := OrderedCollection new.
	newCollection do: [:item | 
		priorMorph := self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
	].
	scroller addAllMorphs: morphList after: parentMorph.
	^morphList
	

]

{ #category : #actions }
SimpleHierarchicalListMorph >> adjustSubmorphPositions [
	"Fixed to not require setting item widths to 9999."
	
	| p |
	p := 0@0.
	scroller submorphsDo: [ :each | | h |
		h := each height.
		each privateBounds: (p extent: each width@h).
		p := p + (0@h)
	].
	self 
		changed;
		layoutChanged;
		setScrollDeltas.

]

{ #category : #actions }
SimpleHierarchicalListMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self selectionColor: self selectionColor.
	self borderStyle baseColor: paneColor twiceDarker
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> allItems [

	^ (submorphs detect:[:each | each class = TransformMorph ]) submorphs
]

{ #category : #'keyboard navigation' }
SimpleHierarchicalListMorph >> arrowKey: aChar [
	"Handle a keyboard navigation character. Answer true if handled, false if not."

	| keyEvent |
	keyEvent := aChar asciiValue.
	keyEvent = 31
		ifTrue: [ 
			"down"
			self setSelectionIndex: self getSelectionIndex + 1.
			^ true ].
	keyEvent = 30
		ifTrue: [ 
			"up"
			self setSelectionIndex: (self getSelectionIndex - 1 max: 1).
			^ true ].
	keyEvent = 1
		ifTrue: [ 
			"home"
			self setSelectionIndex: 1.
			^ true ].
	keyEvent = 4
		ifTrue: [ 
			"end"
			self setSelectionIndex: scroller submorphs size.
			^ true ].
	keyEvent = 11
		ifTrue: [ 
			"page up"
			self setSelectionIndex: (self getSelectionIndex - self numSelectionsInView max: 1).
			^ true ].
	keyEvent = 12
		ifTrue: [ 
			"page down"
			self setSelectionIndex: self getSelectionIndex + self numSelectionsInView.
			^ true ].
	keyEvent = 29
		ifTrue: [ 
			"right"
			selectedMorph
				ifNotNil: [ 
					selectedMorph canExpand
						ifTrue: [ 
							selectedMorph isExpanded not
								ifTrue: [ self toggleExpandedState: selectedMorph ]
								ifFalse: [ self setSelectionIndex: self getSelectionIndex + 1 ] ] ].
			^ true ].
	keyEvent = 28
		ifTrue: [ 
			"left"
			selectedMorph
				ifNotNil: [ 
					selectedMorph isExpanded
						ifTrue: [ self toggleExpandedState: selectedMorph ]
						ifFalse: [ 
							| i |
							"Search parent."
							selectedMorph indentLevel > 0
								ifTrue: [ 
									i := self getSelectionIndex max: 1.
									[ i > 1 and: [ (scroller submorphs at: i) indentLevel >= selectedMorph indentLevel ] ]
										whileTrue: [ i := i - 1 max: 1 ].
									self setSelectionIndex: i ] ] ].
			^ true ].
	^ false
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> autoDeselect: trueOrFalse [
	"Enable/disable autoDeselect (see class comment)"
	autoDeselect := trueOrFalse.
]

{ #category : #search }
SimpleHierarchicalListMorph >> basicKeyPressed: aChar [
	| nextSelection milliSeconds slowKeyStroke oldSelection list |
	nextSelection := oldSelection := lastSelection.
	milliSeconds := Time millisecondClockValue.
	slowKeyStroke := milliSeconds - lastKeystrokeTime > 500.
	lastKeystrokeTime := milliSeconds.
	self searchedElement: nil.
	slowKeyStroke
		ifTrue: [ 
			"forget previous keystrokes and search in following elements"
			lastKeystrokes := aChar asLowercase asString ]
		ifFalse: [ 
			"append quick keystrokes but don't move selection if it still matches"
			lastKeystrokes := lastKeystrokes , aChar asLowercase asString ].	"Get rid of blanks and style used in some lists"	"No change if model is locked"
	model okToChange
		ifFalse: [ ^ self ].
	list := self allItems.
	list
		detect: [ :a | a searchingString trimBoth asLowercase beginsWith: lastKeystrokes ]
		ifFound: [ :nextSelectionNode | 
			nextSelection := list indexOf: nextSelectionNode ifAbsent: [ 0 ].	"The following line is a workaround around the behaviour of OBColumn>>selection:,
	 which deselects when called twice with the same argument."
			self searchedElement: nextSelectionNode.
			lastSelection := nextSelection.	"change scrollbarvalue"
			^ self vScrollValue: (nextSelection - 1) / list size ]
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> collapseAll [
	"Expand all of the roots!"
	
	self roots do: [:m |
		self collapseAll: m].
	self adjustSubmorphPositions
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> collapseAll: aMorph [
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		each isExpanded
			ifTrue: [self collapseAll: each]].

]

{ #category : #accessing }
SimpleHierarchicalListMorph >> columns [

	^columns
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> columns: anArray [

	columns := anArray
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> currentlyExpanded [

	^(scroller submorphs select: [ :each | each isExpanded]) collect: [ :each |
		each complexContents
	].
	
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> defaultColor [
	^ self theme backgroundColor
]

{ #category : #drawing }
SimpleHierarchicalListMorph >> drawLinesOn: aCanvas [
	"Draw the lines for the submorphs.
	Modified for performance."
	
	| lColor|
	lColor := self lineColor.
	aCanvas 
		transformBy: scroller transform
		clippingTo: scroller innerBounds
		during: [:clippedCanvas |
			scroller submorphs do: [ :submorph | | last |
				((submorph isExpanded and: [
					(submorph nextSibling notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: submorph nextSibling fullBounds bottomRight)]) or: [
					submorph nextSibling isNil and: [(last := submorph lastChild) notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: last fullBounds bottomRight)]]]]) or: [
				(clippedCanvas isVisible: submorph fullBounds) or: [
				(submorph nextSibling notNil and: [
						clippedCanvas isVisible: submorph nextSibling fullBounds])]]) ifTrue:[
					submorph drawLinesOn: clippedCanvas lineColor: lColor]]]
		smoothing: scroller smoothing

]

{ #category : #drawing }
SimpleHierarchicalListMorph >> drawOn: aCanvas [
	"Draw the selection and lines."
	
	super drawOn: aCanvas.
	searchedElement ifNotNil:
		[aCanvas clipBy: self innerBounds during: [:c |
			c
				fillRectangle: self searchedFrame
				color: (self secondarySelectionColor ifNil: [self theme secondarySelectionColor])]].
	selectedMorph ifNotNil:
		[aCanvas clipBy: self innerBounds during: [:c |
			c
				fillRectangle: self selectionFrame
				color: (self selectionColorToUse ifNil: [ self theme selectionColor ]) ] ].
	self drawLinesOn: aCanvas
]

{ #category : #events }
SimpleHierarchicalListMorph >> expand: aMorph to: level [
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		((each canExpand
			and: [each isExpanded not])
			and: [level > 0])
			ifTrue: [self expand: each to: level-1]].
]

{ #category : #actions }
SimpleHierarchicalListMorph >> expandAll [
	"Expand all of the roots!"
	
	self roots do: [:m |
		self expandAll: m].
	self adjustSubmorphPositions
]

{ #category : #events }
SimpleHierarchicalListMorph >> expandAll: aMorph [
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		(each canExpand and: [each isExpanded not])
			ifTrue: [self expandAll: each]].

]

{ #category : #events }
SimpleHierarchicalListMorph >> expandAll: aMorph except: aBlock [
	| allChildren |
	(aBlock value: aMorph complexContents)
		ifFalse: [^self].
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		(each canExpand
			and: [each isExpanded not])
			ifTrue: [self expandAll: each except: aBlock]].
]

{ #category : #events }
SimpleHierarchicalListMorph >> expandRoots [
	"Expand all the receiver's roots"
	self roots
		do: [:each |
			(each canExpand and: [each isExpanded not])
				ifTrue: [each toggleExpandedState]].
	self adjustSubmorphPositions
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> expandedForm [
	"Answer the form to use for expanded items."
	
	^self theme treeExpandedForm
]

{ #category : #actions }
SimpleHierarchicalListMorph >> expandedFormForMorph: aMorph [
	"Answer the form to use for expanded items."
	
	^ (self selectedMorph = aMorph and: [self theme selectionColor luminance < 0.6])
		ifTrue: [self theme whiteTreeExpandedForm]
		ifFalse: [self theme treeExpandedForm]
]

{ #category : #geometry }
SimpleHierarchicalListMorph >> extent: newExtent [
	bounds extent = newExtent ifTrue: [^ self].
	super extent: newExtent.
	self setScrollDeltas 
]

{ #category : #selection }
SimpleHierarchicalListMorph >> getCurrentSelectionItem [

	^model perform: (getSelectionSelector ifNil: [^nil])
	
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> getList [ 
	"Answer the list to be displayed."

	^(model perform: (getListSelector ifNil: [^#()])) ifNil: [#()]


]

{ #category : #accessing }
SimpleHierarchicalListMorph >> getListSelector [
	"Answer for compatibility with PluggableListMorph."
	
	^ getListSelector
]

{ #category : #'keyboard navigation' }
SimpleHierarchicalListMorph >> getSelectionIndex [
	^scroller submorphs indexOf: selectedMorph
]

{ #category : #scrolling }
SimpleHierarchicalListMorph >> hExtraScrollRange [
	"Return the amount of extra blank space to include below the bottom of the scroll content."
	^5

]

{ #category : #'events-processing' }
SimpleHierarchicalListMorph >> handleMouseMove: anEvent [
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

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> handlesMouseOverDragging: evt [
	^self dropEnabled
]

{ #category : #drawing }
SimpleHierarchicalListMorph >> highlightSelection [

	selectedMorph ifNotNil: [selectedMorph highlight]
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> indentingItemClass [
	
	^IndentingListItemMorph
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> initForKeystrokes [
	lastKeystrokeTime := 0.
	lastKeystrokes := ''.
	lastSelection := 0
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	self initForKeystrokes.
	self
		on: #mouseMove
		send: #mouseStillDown:onItem:
		to: self
]

{ #category : #keymapping }
SimpleHierarchicalListMorph >> initializeShortcuts: aKMDispatcher [

	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #SimpleHierarchicalList.
	aKMDispatcher attachCategory: #MorphFocusNavigation
]

{ #category : #private }
SimpleHierarchicalListMorph >> insertNewMorphs: morphList [

	scroller addAllMorphs: morphList.
	self adjustSubmorphPositions.
	self selection: self getCurrentSelectionItem.
	self setScrollDeltas.

]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> keyStroke: event [

	"Process potential command keys that are not handled by keymappings yet"

	| args aCharacter |

	aCharacter := event keyValue asCharacter.
	event anyModifierKeyPressed
		ifFalse: [ self basicKeyPressed: aCharacter.
			^ false
			].
	( self scrollByKeyboard: event )
		ifTrue: [ ^ true ].
	keystrokeActionSelector ifNil: [ ^ false ].
	( args := keystrokeActionSelector numArgs ) = 1
		ifTrue: [ ^ model perform: keystrokeActionSelector with: aCharacter ].
	args = 2
		ifTrue: [ ^ model perform: keystrokeActionSelector with: aCharacter with: self ].
	^ self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'
]

{ #category : #updating }
SimpleHierarchicalListMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> lineColor [
	"Answer a good color to use for drawing the lines that connect members of the hierarchy view.
	Used the cached color, or derive it if necessary by finding the receiver or the first owner (up to my root) that is not transparent, then picking a contrasting color.
	Fall back to black if all my owners are transparent."

	| colored |
	lineColor ifNotNil: [^lineColor ].
	colored := self color isTransparent
		ifTrue: [self firstOwnerSuchThat: [:o | o isWorldOrHandMorph not and: [o color isTransparent not]]]
		ifFalse: [self].
	colored ifNil: [^Color black].
	^colored color luminance > 0.5
		ifTrue: [Color black]
		ifFalse: [Color white]
	

]

{ #category : #accessing }
SimpleHierarchicalListMorph >> lineColor: aColor [
	^lineColor := aColor

]

{ #category : #initialization }
SimpleHierarchicalListMorph >> list: aCollection [

	| wereExpanded morphList |
	wereExpanded := self currentlyExpanded.
	scroller removeAllMorphs.
	(aCollection isNil or: [aCollection isEmpty]) ifTrue: [^ self selectedMorph: nil].
	morphList := OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aCollection 
		allowSorting: false
		withExpandedItems: wereExpanded
		atLevel: 0.
	self insertNewMorphs: morphList.
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> listItemHeight [
	"This should be cleaned up.  The list should get spaced by this parameter."
	^ 12
]

{ #category : #selection }
SimpleHierarchicalListMorph >> maximumSelection [

	^ scroller submorphs size

]

{ #category : #selection }
SimpleHierarchicalListMorph >> minimumSelection [
	^ 1
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseDown: evt [
	"Changed to take keybaord focus."
	
	| aMorph selectors |
	self wantsKeyboardFocus
		ifTrue: [self takeKeyboardFocus].
	aMorph := self itemFromPoint: evt position.
	(aMorph notNil and:[aMorph inToggleArea: (aMorph point: evt position from: self)])
		ifTrue:[^self toggleExpandedState: aMorph event: evt]. 
	
	evt yellowButtonPressed  "First check for option (menu) click"
		ifTrue: [ 
			(self yellowButtonActivity: evt shiftPressed) 
				ifTrue: [ ^ self ]].
		
	aMorph ifNil:[^super mouseDown: evt].
	aMorph highlightForMouseDown.
	selectors := Array 
		with: #click:
		with: nil
		with: nil
		with: (self dragEnabled ifTrue:[#startDrag:] ifFalse:[nil]).
	evt hand waitForClicksOrDrag: self event: evt selectors: selectors threshold: 10 "pixels".
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> mouseDownHighlightColor [
	"Answer a good color to use for drawing the mouse down highlight.
	Used the line color if not transparent, otherwise a contrasting color in the
	same way as the line color is determined.
	Fall back to black if all my owners are transparent."

	|c colored |
	c := self lineColor.
	c isTransparent ifFalse: [^c].
	colored := self color isTransparent
		ifTrue: [self firstOwnerSuchThat: [:o | o isWorldOrHandMorph not and: [o color isTransparent not]]]
		ifFalse: [self].
	colored ifNil: [^Color black].
	^colored color luminance > 0.5
		ifTrue: [Color black]
		ifFalse: [Color white]

]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseEnter: event [
	"Changed to take keyboardFocusOnMouseDown into account."
	
	super mouseEnter: event.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown
		ifFalse: [self takeKeyboardFocus]
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseEnterDragging: evt [
	| aMorph |
	(evt hand hasSubmorphs and:[self dropEnabled]) ifFalse: ["no d&d"
		^super mouseEnterDragging: evt].
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt )
		ifTrue:[
			aMorph := self itemFromPoint: evt position.
			aMorph ifNotNil:[self potentialDropMorph: aMorph].
			evt hand newMouseFocus: self.
			"above is ugly but necessary for now"
		].
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseLeaveDragging: anEvent [
	(self dropEnabled and:[anEvent hand hasSubmorphs]) ifFalse: ["no d&d"
		^ super mouseLeaveDragging: anEvent].
	self resetPotentialDropMorph.
	anEvent hand releaseMouseFocus: self.
	"above is ugly but necessary for now"

]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseMove: evt [

	|aMorph|
	aMorph := self itemFromPoint: evt position.
	evt hand hasSubmorphs ifFalse: [
		(aMorph isNil or: [aMorph highlightedForMouseDown not])
			ifTrue: [scroller submorphsDo: [:m |
					m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
					aMorph ifNotNil: [aMorph highlightForMouseDown]]].
	(self dropEnabled and:[evt hand hasSubmorphs]) 
		ifFalse:[^super mouseMove: evt].
	potentialDropMorph ifNotNil:[
		(potentialDropMorph containsPoint: (potentialDropMorph point: evt position from: self))
			ifTrue:[^self].
	].
	self mouseLeaveDragging: evt.
	(self containsPoint: evt position) 
		ifTrue:[self mouseEnterDragging: evt].
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> mouseUp: event [ 
	"Fixed up highlight problems."
	
	| aMorph wasHigh|
	aMorph := self itemFromPoint: event position.
	wasHigh := aMorph notNil ifTrue: [aMorph highlightedForMouseDown] ifFalse: [false].
	scroller submorphsDo: [:m |
		m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
	aMorph ifNil: [^self].
	wasHigh ifFalse: [^self].
	model okToChange ifFalse: [^self].
	"No change if model is locked"
	((autoDeselect == true) and: [aMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: aMorph].
	Cursor normal show
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> notExpandedForm [
	"Answer the form to use for unexpanded items."
	
	^self theme treeUnexpandedForm
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> notExpandedFormForMorph: aMorph [
	^ (self selectedMorph = aMorph and: [self theme selectionColor luminance < 0.6])
		ifTrue: [self theme whiteTreeUnexpandedForm]
		ifFalse: [self theme treeUnexpandedForm]

]

{ #category : #private }
SimpleHierarchicalListMorph >> noteRemovalOfAll: aCollection [

	scroller removeAllMorphsIn: aCollection.
	(aCollection includes: selectedMorph) ifTrue: [self setSelectedMorph: nil].
]

{ #category : #initialization }
SimpleHierarchicalListMorph >> on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel [

	self model: anObject.
	getListSelector := getListSel.
	getSelectionSelector := getSelectionSel.
	setSelectionSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	autoDeselect := true.
	self borderWidth: 1.
	self list: self getList.
]

{ #category : #'dropping/grabbing' }
SimpleHierarchicalListMorph >> potentialDropMorph [
	^potentialDropMorph
]

{ #category : #'dropping/grabbing' }
SimpleHierarchicalListMorph >> potentialDropMorph: aMorph [
	potentialDropMorph := aMorph.
	aMorph highlightForDrop
]

{ #category : #'dropping/grabbing' }
SimpleHierarchicalListMorph >> resetPotentialDropMorph [
	potentialDropMorph ifNotNil: [
		potentialDropMorph resetHighlightForDrop.
		potentialDropMorph := nil]

]

{ #category : #accessing }
SimpleHierarchicalListMorph >> roots [
	"Answer the receiver's roots"
	^ scroller submorphs
		select: [:each | each indentLevel isZero]
]

{ #category : #geometry }
SimpleHierarchicalListMorph >> scrollDeltaHeight [
	^ scroller firstSubmorph height
]

{ #category : #geometry }
SimpleHierarchicalListMorph >> scrollDeltaWidth [
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2



]

{ #category : #accessing }
SimpleHierarchicalListMorph >> searchedElement [

	^ searchedElement
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> searchedElement: anElement [

	searchedElement := anElement.
]

{ #category : #search }
SimpleHierarchicalListMorph >> searchedFrame [
	"Answer the frame of the selected morph in the receiver or nil if none."
	
	^searchedElement
		ifNotNil: [:elt | elt bounds: elt selectionFrame in: self]
]

{ #category : #search }
SimpleHierarchicalListMorph >> secondarySelectionColor [
	^ self theme secondarySelectionColor
]

{ #category : #selection }
SimpleHierarchicalListMorph >> selectedMorph [
	^selectedMorph
]

{ #category : #selection }
SimpleHierarchicalListMorph >> selectedMorph: aMorph [

	self unhighlightSelection.
	selectedMorph := aMorph.
	self highlightSelection
]

{ #category : #selection }
SimpleHierarchicalListMorph >> selection: item [
	"Called from outside to request setting a new selection.
	Assumes scroller submorphs is exactly our list.
	Note: MAY NOT work right if list includes repeated items"

	| i |
	item ifNil: [^self selectionIndex: 0].
	i := scroller submorphs findFirst: [:m | m complexContents == item].
	i > 0 ifTrue: [^self selectionIndex: i].
	i := scroller submorphs findFirst: [:m | m withoutListWrapper = item withoutListWrapper].
	self selectionIndex: i
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> selectionColor [
	"Answer the colour to use for selected items."

	^self valueOfProperty: #selectionColor ifAbsent: [] 
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> selectionColor: aColor [
	"Set the colour for selected items."

	| window |
	aColor
		ifNil: [ self removeProperty: #selectionColor ]
		ifNotNil: [ self setProperty: #selectionColor toValue: aColor ].
		
	window := self ownerThatIsA: SystemWindow.
	
	self selectionColorToUse: ((self theme fadedBackgroundWindows not or: [ window isNil or: [ window isActive ] ])
		ifTrue: [ aColor ]
		ifFalse: [ self theme unfocusedSelectionColor ])
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> selectionColorToUse [
	"Answer the colour to use for selected items."

	^self 
		valueOfProperty: #selectionColorToUse 
		ifAbsent: [ self theme selectionColor ]
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> selectionColorToUse: aColor [
	"Set the colour for selected items."

	aColor = self selectionColorToUse ifTrue: [^self].
	aColor
		ifNil: [self removeProperty: #selectionColorToUse]
		ifNotNil: [self setProperty: #selectionColorToUse toValue: aColor].
	self selectionFrameChanged
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> selectionFrame [
	"Answer the frame of the selected morph in the receiver or nil if none."
	
	^selectedMorph
		ifNotNil: [selectedMorph bounds:  selectedMorph selectionFrame in: self]
]

{ #category : #updating }
SimpleHierarchicalListMorph >> selectionFrameChanged [
	"Invalidate frame of the current selection if any."
	
	selectedMorph ifNil: [ ^self ].
	self invalidRect: self selectionFrame
]

{ #category : #selection }
SimpleHierarchicalListMorph >> selectionIndex: idx [
	"Called internally to select the index-th item."
	| theMorph range index |
	idx ifNil: [^ self].
	index := idx min: scroller submorphs size max: 0.
	(theMorph := index = 0 ifTrue: [nil] ifFalse: [scroller submorphs at: index])
		ifNotNil:
		[((theMorph bounds top - scroller offset y) >= 0
			and: [(theMorph bounds bottom - scroller offset y) <= self innerBounds height]) ifFalse:
			["Scroll into view -- should be elsewhere"
			range := self vTotalScrollRange.
			scrollBar value: (range > 0
				ifTrue: [((index-1 * theMorph height) / self vTotalScrollRange)
									truncateTo: scrollBar scrollDelta]
				ifFalse: [0]).
			scroller offset: -3 @ (range * scrollBar value)]].
	self selectedMorph: theMorph
]

{ #category : #selection }
SimpleHierarchicalListMorph >> selectionOneOf: aListOfItems [
	"Set the selection to the first item in the list which is represented by one of my submorphs"

	
	aListOfItems do: [ :item | | index |
		index := scroller submorphs findFirst: [:m | 
			m withoutListWrapper = item withoutListWrapper
		].
		index > 0 ifTrue: [^self selectionIndex: index].
	].
	self selectionIndex: 0.
]

{ #category : #selection }
SimpleHierarchicalListMorph >> setSelectedMorph: aMorph [

	model 
		perform: (setSelectionSelector ifNil: [^self]) 
		with: aMorph complexContents	"leave last wrapper in place"

 
]

{ #category : #'keyboard navigation' }
SimpleHierarchicalListMorph >> setSelectionIndex: idx [
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index := idx min: scroller submorphs size max: 0.
	theMorph := index = 0 ifTrue: [nil] ifFalse: [scroller submorphs at: index].
	self setSelectedMorph: theMorph.
]

{ #category : #accessing }
SimpleHierarchicalListMorph >> sortingSelector: s [

	sortingSelector := s
]

{ #category : #'event handling' }
SimpleHierarchicalListMorph >> startDrag: anEvent [ 
	| aTransferMorph itemMorph passenger |
	self dragEnabled
		ifTrue: [itemMorph := scroller submorphs
						detect: [:any | any highlightedForMouseDown]
						ifNone: []].
	(itemMorph isNil
			or: [anEvent hand hasSubmorphs])
		ifTrue: [^ self].
	itemMorph highlightForMouseDown: false.
	itemMorph ~= self selectedMorph
		ifTrue: [self setSelectedMorph: itemMorph].
	passenger := self model dragPassengerFor: itemMorph inMorph: self.
	passenger
		ifNotNil: [
			aTransferMorph := self model transferFor: passenger from: self.
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			aTransferMorph
				dragTransferType: (self model dragTransferTypeForMorph: self).
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self
]

{ #category : #testing }
SimpleHierarchicalListMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #'keyboard navigation' }
SimpleHierarchicalListMorph >> toggleExpandedState: aMorph [
	aMorph toggleExpandedState.
	self adjustSubmorphPositions.

]

{ #category : #events }
SimpleHierarchicalListMorph >> toggleExpandedState: aMorph event: event [
	| oldState |
	"self setSelectedMorph: aMorph."
	event yellowButtonPressed ifTrue: [
		oldState := aMorph isExpanded.
		scroller submorphs copy do: [ :each |
			(each canExpand and: [each isExpanded = oldState]) ifTrue: [
				each toggleExpandedState.
			].
		].
	] ifFalse: [
		aMorph toggleExpandedState.
	].
	self adjustSubmorphPositions.
	
]

{ #category : #drawing }
SimpleHierarchicalListMorph >> unhighlightSelection [
	selectedMorph ifNotNil: [selectedMorph unhighlight]
]

{ #category : #updating }
SimpleHierarchicalListMorph >> update: aSymbol [ 
	aSymbol == getSelectionSelector 
		ifTrue: 
			[self selection: self getCurrentSelectionItem.
			^self].
	aSymbol == getListSelector 
		ifTrue: 
			[self list: self getList.
			^self].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size > 1 and: [aSymbol first == getListSelector and: [
					aSymbol second == #openPath]]]) "allow directed path opening where multiple trees exist"
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openPath: (aSymbol allButFirst: 2)].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size > 1 and: [aSymbol first == getListSelector and: [
					aSymbol second == #openItemPath]]]) "allow directed path opening where multiple trees exist"
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openItemPath: (aSymbol allButFirst: 2)].
	((aSymbol isKindOf: Array) 
		and: [aSymbol notEmpty and: [aSymbol first == #openPath]]) 
			ifTrue: 
				[^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openPath: aSymbol allButFirst].
	((aSymbol isKindOf: Array) 
		and: [aSymbol size  = 2 and: [aSymbol first = getListSelector and: [
					aSymbol second == #expandRoots]]]) 
			ifTrue: 
				[^self expandRoots].
	((aSymbol isKindOf: Array) 
		and: [aSymbol notEmpty and: [aSymbol first = getListSelector and: [
					aSymbol second == #expandAll]]]) 
			ifTrue: 
				[^self expandAll]
]

{ #category : #scrolling }
SimpleHierarchicalListMorph >> vUnadjustedScrollRange [
"Return the width of the widest item in the list"

	(scroller submorphs size > 0) ifFalse:[ ^0 ].
	^scroller submorphs last fullBounds bottom

]

{ #category : #'dropping/grabbing' }
SimpleHierarchicalListMorph >> wantsDroppedMorph: aMorph event: anEvent [ 
	^ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self
]
