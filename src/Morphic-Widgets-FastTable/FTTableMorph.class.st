"
I'm an implementation of a table, in a not-naive way. 

I assume I can have many rows, then I do not try to show all of them at once. Instead, I keep a datasource and I demand rows when needed (datasource implements a flyweight to fill the visible rows).

I should not be subclasse. An extension of FastTable should happen on a data source an not here. Extend me ONLY if it is impossible to do what you want on the data source.

Examples: 
-------------
FTTableMorph new
	extent: 200@400;
	dataSource: (FTSimpleDataSource elements: (1 to: 10000) );
	openInWindow.
	
You can check better examples in FTExamples

A FastTable have the possibility to be searchable, this is activate by default. 
You can disable this with the method #disableSearch.
But you also have the possibility to make your FastTable filterable with the method #enableFilter. But search and filter cannot be use in the same time.
"
Class {
	#name : #FTTableMorph,
	#superclass : #Morph,
	#instVars : [
		'container',
		'verticalScrollBar',
		'horizontalScrollBar',
		'selectionColor',
		'showIndex',
		'dataSource',
		'intercellSpacing',
		'rowHeight',
		'selectedRowIndexes',
		'highlightedRowIndexes',
		'selectionStrategy',
		'columns',
		'secondarySelectionColor',
		'headerColor',
		'showColumnHeaders',
		'allowsDeselection',
		'needToggleAtMouseUp',
		'function',
		'resizable',
		'trialHSB'
	],
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #accessing }
FTTableMorph class >> defaultAllowsDeselection [
	^ false
]

{ #category : #accessing }
FTTableMorph class >> defaultBackgroundColor [
	^ Smalltalk ui theme listBackgroundColor
]

{ #category : #accessing }
FTTableMorph class >> defaultColumn [	
	^ FTColumn new
]

{ #category : #accessing }
FTTableMorph class >> defaultHeaderColor [
	self flag: #todo. "I think we should deprecate all this headerColor stuff. This is 
	responsibility of data source, after all"
	^ Smalltalk ui theme fastTableHeaderColor
]

{ #category : #accessing }
FTTableMorph class >> defaultIntercellSpacing [
	^ 0@0
]

{ #category : #accessing }
FTTableMorph class >> defaultRowHeight [
	^ StandardFonts defaultFont pixelSize + 7
]

{ #category : #accessing }
FTTableMorph class >> defaultSecondarySelectionColor [
	^ Smalltalk ui theme secondarySelectionColor
]

{ #category : #accessing }
FTTableMorph class >> defaultSelectionColor [
	^ Smalltalk ui theme selectionColor
]

{ #category : #temporary }
FTTableMorph class >> newTrialHorizontalScrollBar [
	"This method just temporary to guard new changes from hurtingtools durign development"
	"See GTExamples class >> exampleTableHorizontalScroll"
	^self basicNew initializeTrialHorizontalScrollBar
]

{ #category : #'drag and drop' }
FTTableMorph >> acceptDroppingMorph: aMorph event: event [
	self dataSource 
		dropElements: aMorph passenger 
		index: ((self container rowIndexContainingPoint: event position) ifNil: [ 0 ]).
	self basicHighlightRowIndexes: #().
	self refresh
]

{ #category : #accessing }
FTTableMorph >> addColumn: aColumn [
	columns := columns copyWith: aColumn
]

{ #category : #accessing }
FTTableMorph >> allowDeselection [
	self allowsDeselection: true
]

{ #category : #testing }
FTTableMorph >> allowsDeselection [
	^ allowsDeselection ifNil: [ self class defaultAllowsDeselection ]
]

{ #category : #accessing }
FTTableMorph >> allowsDeselection: aBoolean [
	allowsDeselection := aBoolean
]

{ #category : #updating }
FTTableMorph >> autoScrollHeightLimit [

	^20
]

{ #category : #private }
FTTableMorph >> basicHighlightRowIndexes: anArray [
	highlightedRowIndexes := anArray asArray
]

{ #category : #private }
FTTableMorph >> basicMoveShowIndexTo: aNumber [
	showIndex := aNumber.

]

{ #category : #private }
FTTableMorph >> basicSelectRowIndexes: anArray [
	selectedRowIndexes := anArray asArray.

]

{ #category : #accessing }
FTTableMorph >> beMultipleSelection [
	selectionStrategy := FTMultipleSelection table: self
]

{ #category : #accessing }
FTTableMorph >> beNotResizable [
	resizable := false
]

{ #category : #accessing }
FTTableMorph >> beResizable [
	resizable := true
]

{ #category : #accessing }
FTTableMorph >> beSingleSelection [
	selectionStrategy := FTSimpleSelection table: self
]

{ #category : #'event handling' }
FTTableMorph >> click: event [
	"check for right click (menu)"

	(self isYellowButtonReallyPressed: event)
		ifTrue: [ self showMenuForPosition: event cursorPoint ]
]

{ #category : #'accessing colors' }
FTTableMorph >> colorForSelection: primarySelection [
	
	^primarySelection 
		ifTrue: [ self selectionColor ]
		ifFalse: [ self secondarySelectionColor ]
]

{ #category : #accessing }
FTTableMorph >> columns [
	^ columns
]

{ #category : #accessing }
FTTableMorph >> columns: aCollection [ 
	columns := aCollection asArray
]

{ #category : #private }
FTTableMorph >> container [ 
	^ container
]

{ #category : #accessing }
FTTableMorph >> dataSource [
	"Answers a dataSource: the component responsible of providing data to the table. 
	 Check FTDataSource and subclasses as reference."
	^ dataSource
]

{ #category : #accessing }
FTTableMorph >> dataSource: anObject [
	dataSource := anObject.
	dataSource table: self.
	dataSource readyToBeDisplayed.
	self resetPosition.
	self refresh
]

{ #category : #initialization }
FTTableMorph >> defaultColor [

	^self class defaultBackgroundColor
]

{ #category : #accessing }
FTTableMorph >> defaultContainer [
	^ FTTableContainerMorph new
]

{ #category : #accessing }
FTTableMorph >> denyDeselection [
	self allowsDeselection: false
]

{ #category : #accessing }
FTTableMorph >> disableFunction [
	"Disabling it just sets the funtion to nil, so I can safely skip it in #keyStrokeSearch:"

	function isExplicite
		ifTrue: [ function disable.
			self resizeAllSubviews	"This is call because now the container will take all the available space." ].
	function := FTNilFunction table: self
]

{ #category : #'event handling' }
FTTableMorph >> doubleClick: event [
	| rowIndex |
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	rowIndex ifNotNil: [ 
		self doAnnounce: (FTStrongSelectionChanged rowIndex: rowIndex event: event) ]
]

{ #category : #drawing }
FTTableMorph >> drawSubmorphsOn: aCanvas [ 
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."
	"1haltOnce."
	super drawSubmorphsOn: aCanvas.
	self hasKeyboardFocus ifTrue: [ self drawKeyboardFocusOn: aCanvas ]

]

{ #category : #accessing }
FTTableMorph >> enableFilter [
	"Enables filtering. Not compatible with the search."

	function := FTFilterFunction table: self
]

{ #category : #accessing }
FTTableMorph >> enableFilter: aFTFilterClass [
	"Enables filtering. Not compatible with the search."

	self enableFilter.
	function filterClass: aFTFilterClass
]

{ #category : #accessing }
FTTableMorph >> enableFilterWithAction: aBlock [
	"Enables filtering and add an Action button. Not compatible with the search."

	self enableFilterWithAction: aBlock named: 'Validate.'
]

{ #category : #accessing }
FTTableMorph >> enableFilterWithAction: aBlock named: aString [
	"Enables filtering and add an Action button. Not compatible with the search. If I am use, the function must be explicit."

	function := FTFilterFunctionWithAction table: self action: aBlock named: aString.
	self explicitFunction
]

{ #category : #accessing }
FTTableMorph >> enableSearch [
	"Enables search (this is the default option). Not compatible with the filter function.	"

	function := FTSearchFunction table: self
]

{ #category : #private }
FTTableMorph >> ensureAtLeastOneColumn [
	self columns ifNotEmpty: [ ^ self ].
	self addColumn: self class defaultColumn
]

{ #category : #private }
FTTableMorph >> ensureVisibleFirstSelection [
	| rowIndex |
	self hasSelection ifFalse: [ ^ self ].
	rowIndex := self selectedRowIndex.
	(self container isRowIndexFullyVisible: rowIndex) ifFalse: [ 
		rowIndex < self showIndex 
			ifTrue: [ self moveShowIndexTo: rowIndex ]
			ifFalse: [ self moveShowIndexTo: (rowIndex - self container calculateMinVisibleRows + 1) ] ]
]

{ #category : #accessing }
FTTableMorph >> explicitFunction [
	function showWidget
]

{ #category : #geometry }
FTTableMorph >> extent: aPoint [
	super extent: aPoint.
	container extent: aPoint.
	self resizeAllSubviews
]

{ #category : #'event handling' }
FTTableMorph >> handleMouseMove: anEvent [
	"Reimplemented because we really want #mouseMove when a morph is dragged around"
	anEvent wasHandled ifTrue:[^self]. "not interested"
	(anEvent anyButtonPressed) ifFalse:[^self].
	anEvent wasHandled: true.
	self mouseMove: anEvent

]

{ #category : #'event testing' }
FTTableMorph >> handlesKeyboard: event [
	^ true
]

{ #category : #'event testing' }
FTTableMorph >> handlesMouseDown: event [
	^ true

]

{ #category : #'event testing' }
FTTableMorph >> handlesMouseOverDragging: event [
	"Yes, for mouse down highlight."
	^true
]

{ #category : #'event testing' }
FTTableMorph >> handlesMouseWheel: event [
	^self isVerticalScrollBarVisible and: [ self hasDataSource ]
]

{ #category : #testing }
FTTableMorph >> hasDataSource [
	^ self dataSource notNil
]

{ #category : #testing }
FTTableMorph >> hasHighlighted [
	^ self highlightedRowIndexes notEmpty
]

{ #category : #testing }
FTTableMorph >> hasSelection [
	^ self selectedRowIndexes notEmpty
]

{ #category : #'accessing colors' }
FTTableMorph >> headerColor [
	^ headerColor ifNil: [ self class defaultHeaderColor ]
]

{ #category : #'accessing colors' }
FTTableMorph >> headerColor: aColor [
	headerColor := aColor
]

{ #category : #accessing }
FTTableMorph >> hideColumnHeaders [
	showColumnHeaders ifFalse: [ ^ self ].
	showColumnHeaders := false.
	self refresh
]

{ #category : #'accessing selection' }
FTTableMorph >> highlightRowIndex: aNumber [
	self highlightRowIndexes: { aNumber }
]

{ #category : #'accessing selection' }
FTTableMorph >> highlightRowIndexes: anArray [
	anArray = highlightedRowIndexes ifTrue: [ ^ self ].
	
	self basicHighlightRowIndexes: anArray. 
	
	(self hasHighlighted and: [ (self isRowIndexVisible: self highlightedRowIndex) not ]) 
		ifTrue: [ 
			self moveShowIndexTo: self highlightedRowIndexes first.
			^ self ].
	
	(self hasSelection and: [ (self isRowIndexVisible: self selectedRowIndex) not  ])
		ifTrue: [ 
			self moveShowIndexTo: self selectedRowIndex.
			^ self ].
	
	self refresh.
]

{ #category : #'accessing selection' }
FTTableMorph >> highlightedRowIndex [
	^ self highlightedRowIndexes 
		ifNotEmpty: [ :indexes | indexes first ]
		ifEmpty: [ 0 ] 
]

{ #category : #'accessing selection' }
FTTableMorph >> highlightedRowIndexes [
	^ highlightedRowIndexes
]

{ #category : #private }
FTTableMorph >> horizontalScrollBar [
	^ horizontalScrollBar
]

{ #category : #private }
FTTableMorph >> horizontalScrollBarHeight [
	^trialHSB 
		ifNil: [ 0 ] 
		ifNotNil: [ 
			self isHorizontalScrollBarVisible ifFalse: [ ^ 0 ].
			self scrollBarThickness
			] 
]

{ #category : #private }
FTTableMorph >> horizontalScrollBarValue: aNumber [
	trialHSB ifNotNil: [ 
		self inform: 'FTTableMorph>>#horizontalScrollBarValue: ', aNumber printString , String cr, 
			'Does nothing yet.' , String cr, 'Need some advise here.'
	].
]

{ #category : #initialization }
FTTableMorph >> initialize [ 
	super initialize.
	showIndex := 0.
	showColumnHeaders := true.
	selectedRowIndexes := #().
	highlightedRowIndexes := #().
	columns := #().
	needToggleAtMouseUp := false.
	self beNotResizable.
	self beSingleSelection.
	self enableSearch.
	self initializeScrollBars.
	self initializeContainer.
	self initializeKeyBindings.
	
	self resizeAllSubviews
]

{ #category : #initialization }
FTTableMorph >> initializeContainer [
	container := self defaultContainer.
	self addMorph: container
]

{ #category : #initialization }
FTTableMorph >> initializeKeyBindings [
	"add keybindings used by table"
	self 
		bindKeyCombination: Character arrowUp shift | Character arrowUp asKeyCombination
		toAction: [ :target :morph :event | self keyStrokeArrowUp: event ].
	self 
		bindKeyCombination: Character arrowDown shift | Character arrowDown asKeyCombination
		toAction: [ :target :morph :event | self keyStrokeArrowDown: event ].
	self 
		bindKeyCombination: Character home asKeyCombination
		toAction: [ self selectRowIndex: 1 ].
	self 
		bindKeyCombination: Character end asKeyCombination
		toAction: [ self selectRowIndex: self numberOfRows ].
	self 
		bindKeyCombination: $a meta  
		toAction: [ self selectAllRowIndexes ]
]

{ #category : #initialization }
FTTableMorph >> initializeScrollBars [
	verticalScrollBar := ScrollBar new 
		model: self; 
		setValueSelector: #verticalScrollBarValue:;
		yourself.
	self addMorph: verticalScrollBar.

	trialHSB ifNotNil: [ 	
		horizontalScrollBar := ScrollBar new 
			model: self; 
			setValueSelector: #horizontalScrollBarValue:;
			yourself.
		self addMorph: horizontalScrollBar
		].
]

{ #category : #initialization }
FTTableMorph >> initializeTrialHorizontalScrollBar [
	trialHSB := true.
	self initialize.
]

{ #category : #accessing }
FTTableMorph >> intercellSpacing [ 
	^ intercellSpacing ifNil: [ self class defaultIntercellSpacing ]
]

{ #category : #accessing }
FTTableMorph >> intercellSpacing: aNumberOrPoint [
	"Determines cell spacing 
		x: space between cells 
		y: space between rows"
	intercellSpacing := aNumberOrPoint asPoint 
]

{ #category : #private }
FTTableMorph >> isHorizontalScrollBarVisible [
	^trialHSB 
		ifNil: [ false ] 
		ifNotNil: [ 
			self horizontalScrollBar owner isNotNil.
			]
]

{ #category : #testing }
FTTableMorph >> isMultipleSelection [
	^ selectionStrategy isMultiple
]

{ #category : #testing }
FTTableMorph >> isResizable [
	^ resizable
]

{ #category : #testing }
FTTableMorph >> isRowIndexSelected: rowIndex [
	^ self selectedRowIndexes includes: rowIndex
]

{ #category : #testing }
FTTableMorph >> isRowIndexVisible: rowIndex [
	^ self container isRowIndexVisible: rowIndex
]

{ #category : #testing }
FTTableMorph >> isShowColumnHeaders [
	^ showColumnHeaders
]

{ #category : #private }
FTTableMorph >> isVerticalScrollBarVisible [
	^ self verticalScrollBar owner isNotNil
]

{ #category : #private }
FTTableMorph >> isYellowButtonReallyPressed: anEvent [
	anEvent yellowButtonPressed ifFalse: [ ^false ].
	"this is shitty fix for VM bug. 
	Now if you will press left mouse button together with pressed cmd (on Mac)
	then you will got right mouse button event.
	Interesting that it is not a problem in case of external SDL2 window.
	Try check it from OSWindowWorldMorph new open"
	^ (Smalltalk os isMacOS) 
		ifTrue: [ anEvent commandKeyPressed not]
		ifFalse: [ anEvent controlKeyPressed not]
]

{ #category : #'event handling' }
FTTableMorph >> keyStroke: event [
	self flag: #todo.	"If the function is explicit this should be redirect to the function widget."
	((super keyStroke: event) or: [ self navigationKey: event ])
		ifTrue: [ ^ true ].
		
	^ self keyStrokeSearch: event
]

{ #category : #'event handling' }
FTTableMorph >> keyStrokeArrowDown: event [
	| rowIndex |
	rowIndex := self selectedRowIndex.
	rowIndex < self numberOfRows
		ifFalse: [ ^ self ].
	self resetFunction.
	self selectionStrategy selectRowIndex: rowIndex + 1 event: event
]

{ #category : #'event handling' }
FTTableMorph >> keyStrokeArrowUp: event [
	| rowIndex |
	rowIndex := self selectedRowIndex.
	rowIndex > 1
		ifFalse: [ ^ self ].
	self resetFunction.
	selectionStrategy selectRowIndex: rowIndex - 1 event: event
]

{ #category : #'event handling' }
FTTableMorph >> keyStrokeSearch: event [
	^ function keyStroke: event
]

{ #category : #'event handling' }
FTTableMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #layout }
FTTableMorph >> minHeight [ 

	^ 100
]

{ #category : #layout }
FTTableMorph >> minWidth [

	^ 100
]

{ #category : #'event handling' }
FTTableMorph >> mouseDown: event [
	| rowIndex |
	"perform the click"
	
	needToggleAtMouseUp ifTrue: [ ^ self ].
	
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	(self selectedRowIndexes includes: rowIndex)
		ifFalse: [ self selectRowIndex: rowIndex event: event ]
		ifTrue: [ needToggleAtMouseUp := true ].	"If the cell is selected we let the mouse up toggle to avoid any problem with the drag and drop"
	self wantsKeyboardFocus
		ifTrue: [ self takeKeyboardFocus ].
	event hand waitForClicksOrDrag: self event: event
]

{ #category : #'event handling' }
FTTableMorph >> mouseEnterDragging: event [
	self enabled ifFalse: [ ^ self ].
	(event hand hasSubmorphs and: [ self dropEnabled ]) 
		ifFalse: [ "no d&d" ^ super mouseEnterDragging: event ].	
]

{ #category : #'event handling' }
FTTableMorph >> mouseLeaveDragging: event [
	"The mouse has left with a button down."
	
	(self dropEnabled and: [event hand hasSubmorphs]) 
		ifFalse: [ "no d&d" ^ super mouseLeaveDragging: event].
		
	self basicHighlightRowIndexes: #().
	self refresh
				
]

{ #category : #'event handling' }
FTTableMorph >> mouseMove: event [
	event isDraggingEvent ifFalse: [ ^ self ].
	event hand hasSubmorphs ifFalse: [ ^self ].
	(self wantsDroppedMorph: event hand submorphs first event: event) ifFalse: [ ^self ].
	
	(self container rowIndexContainingPoint: event position) 
		ifNotNil: [ :rowIndex | 
			self basicHighlightRowIndexes: { rowIndex }.
			self refresh ].
	
	(self container bounds containsPoint: event position) ifTrue: [ 
		(event position y <= (self container top + self autoScrollHeightLimit)) ifTrue: [ 
			^self verticalScrollBar scrollUp: 1
		].
		(event position y >= (self container bottom - self autoScrollHeightLimit)) ifTrue: [ 
			^self verticalScrollBar scrollDown: 1
		] 			
	]

]

{ #category : #'event handling' }
FTTableMorph >> mouseUp: event [
	| rowIndex |
	
	needToggleAtMouseUp ifFalse: [ ^ self ].
	
	"perform the click if the mouse down didn't did it."
	rowIndex := self container rowIndexContainingPoint: event cursorPoint.
	needToggleAtMouseUp := false.
	self selectRowIndex: rowIndex event: event
]

{ #category : #'event handling' }
FTTableMorph >> mouseWheel: event [

	"I tried scrolling up/down with a calculated value (check #scrollUpByPageDelta implementor)
	 but the scrollbar proved been more intelligent than me... looks like hardcoded values 
	 work better :/"
	event isUp ifTrue: [ 
		self verticalScrollBar scrollRestrictedUp: 3.
		^ self
	].
	event isDown ifTrue: [ 
		self verticalScrollBar scrollRestrictedDown: 3.
		^ self
	].
	
	super mouseWheel: event
]

{ #category : #private }
FTTableMorph >> moveShowIndexTo: aNumber [
	"I move the showing index to a specific row, and perform a refresh of subviews. 
	 I should not be used directly, and most methods that need to move the 
	 showing pointer should do it directly. 
	 Use me just in case you need to force a refresh after settign the index"
	self basicMoveShowIndexTo: aNumber.
	self verticalScrollBar value: (self rowIndexToVerticalScrollBarValue: aNumber).
	self refresh
]

{ #category : #accessing }
FTTableMorph >> numberOfColumns [ 
	^ columns size
]

{ #category : #accessing }
FTTableMorph >> numberOfRows [
	self hasDataSource ifFalse: [ ^ 0 ]. 
	^ self dataSource numberOfRows
]

{ #category : #private }
FTTableMorph >> recalculateVerticalScrollBar [
	| interval delta pageDelta visibleRows numberOfRows |
	
	self hasDataSource ifFalse: [ ^ self ].

	self recalculateVerticalScrollBarVisibilityIfHidden: [ ^ self ].
	 
	visibleRows := self container calculateExactVisibleRows.	
	numberOfRows := self dataSource numberOfRows.
	interval := (visibleRows / numberOfRows) asFloat.
	delta := 1/numberOfRows.
	pageDelta := ((visibleRows-1) floor)*delta.
	self verticalScrollBar 
		scrollDelta: delta pageDelta: pageDelta;
		interval: interval
]

{ #category : #private }
FTTableMorph >> recalculateVerticalScrollBarVisibilityIfHidden: aBlock [
	self container calculateExactVisibleRows >= self dataSource numberOfRows
		ifTrue: [ 
			(self isVerticalScrollBarVisible)
				ifTrue: [ self removeMorph: self verticalScrollBar ].
			self resizeContainer. "it changed... I need to resize it immediately because 
			otherwise it does not work fine with first show... this can cause two sends to 
			#resizeContainer but the case is minimal and not expensive, so we can ignore it"
			aBlock value ]
		ifFalse: [ 
			(self isVerticalScrollBarVisible)
				ifFalse: [ 
					self resizeVerticalScrollBar.
					self addMorph: self verticalScrollBar ] ]

]

{ #category : #updating }
FTTableMorph >> refresh [
	"Refreshes all internal values (forces an invalidate of all subviews)"
	self ensureAtLeastOneColumn.
	self recalculateVerticalScrollBar.
	self verticalScrollBar changed.
	trialHSB ifNotNil: [ self horizontalScrollBar changed ].
	self container changed
	
]

{ #category : #accessing }
FTTableMorph >> resetFunction [
	function reset
]

{ #category : #private }
FTTableMorph >> resetPosition [
	"Resets all values to original value"
	showIndex := 0.
	self verticalScrollBar value: 0.
	trialHSB ifNotNil: [	self horizontalScrollBar value: 0 ].
	self container setNeedsRefreshExposedRows.
	self container updateExposedRows.
]

{ #category : #private }
FTTableMorph >> resizeAllSubviews [
	self resizeVerticalScrollBar.
	trialHSB ifNotNil: [ self resizeHorizontalScrollBar ].
	"if we resized scrollbar, we need to recalculate it because values change (and now visibility 
	 can be toggled, shown items can change, etc.)"
	self recalculateVerticalScrollBar.
	self resizeContainer.
	self resizeColumns.
	self container setNeedsRefreshExposedRows.
	self container updateExposedRows.
	self verticalScrollBar value: (self rowIndexToVerticalScrollBarValue: showIndex).
	function isExplicite
		ifTrue: [ function resizeWidget ]
]

{ #category : #private }
FTTableMorph >> resizeColumns [
	"Resize columns just has sense if there is more than one column, otherwise it will be resized 
	 to row size, so I can safely skip the resize part if I have just one (and then I avoid 
	 redundancy)"
	self columns size > 1 ifFalse: [ ^ self ].
	self columns last 
		width: (self width - (self columns allButLast sum: [ :each | each width ifNil: [0]]))
]

{ #category : #private }
FTTableMorph >> resizeContainer [ 
	| topLeft bottomRight |
	topLeft := (self bounds left - self borderWidth) @ self bounds top - self borderWidth.
	bottomRight := (self bounds right  - self verticalScrollBarWidth    - self borderWidth) 
					 @ (self bounds bottom - self horizontalScrollBarHeight - self borderWidth).
	self container
		bounds:
			(function isExplicite
				ifTrue: [ function resizeContainerFrom: topLeft to: bottomRight ]
				ifFalse: [ topLeft corner: bottomRight ])
]

{ #category : #private }
FTTableMorph >> resizeHorizontalScrollBar [
	| width height corner |
	trialHSB ifNotNil: [ 
		width := self bounds width - (self borderWidth * 2) - self verticalScrollBarWidth.
		height := self scrollBarThickness.
		corner := self bounds bottomLeft - ((width + self borderWidth)@(0 - self borderWidth)).
		corner := self bounds bottomLeft - ((0 - self borderWidth)@(height+self borderWidth)).
		self horizontalScrollBar bounds: (corner extent: width@height).
		]
]

{ #category : #private }
FTTableMorph >> resizeVerticalScrollBar [
	| width height corner |
	width := self scrollBarThickness.
	height := self bounds height - (self borderWidth * 2) - self horizontalScrollBarHeight.
	corner := self bounds topRight - ((width + self borderWidth)@(0 - self borderWidth)).
	self verticalScrollBar bounds: (corner extent: width@height)
]

{ #category : #accessing }
FTTableMorph >> rowHeight [
	"This is the row height your rows will have. Cells answered in dataSource will be forced to have
	 this height number... We force it instead allowing lists to have any height because the logic to 
	 calculate rows becomes complicated. Possible, but complicated :)"
	^ rowHeight ifNil: [ rowHeight := self class defaultRowHeight ]
]

{ #category : #accessing }
FTTableMorph >> rowHeight: aNumber [
	rowHeight := aNumber
]

{ #category : #private }
FTTableMorph >> rowIndexToVerticalScrollBarValue: aNumber [
	| numberOfRows |
	numberOfRows := self numberOfRows - self container calculateMinVisibleRows.
	^ (numberOfRows = 0 or: [ aNumber <= 1 ])
		ifTrue: [ 0.0 ]
		ifFalse: [ ((aNumber / numberOfRows) asFloat max: 0.0) min: 1.0 ]
]

{ #category : #private }
FTTableMorph >> scrollBarThickness [
	^ self theme scrollbarThickness
]

{ #category : #'accessing colors' }
FTTableMorph >> secondarySelectionColor [ 
	^ secondarySelectionColor ifNil: [ self class defaultSecondarySelectionColor ]
]

{ #category : #'accessing colors' }
FTTableMorph >> secondarySelectionColor: aColor [
	secondarySelectionColor := aColor
]

{ #category : #'accessing selection' }
FTTableMorph >> selectAllRowIndexes [
	self isMultipleSelection ifFalse: [ ^ self ].
	self selectRowIndexes: (1 to: self numberOfRows) asArray 
]

{ #category : #'accessing selection' }
FTTableMorph >> selectRowIndex: aNumber [
	self selectRowIndexes: { aNumber }
]

{ #category : #private }
FTTableMorph >> selectRowIndex: rowIndex event: event [
	rowIndex
		ifNotNil: [ selectionStrategy selectRowIndex: rowIndex event: event ]
		ifNil: [ self selectRowIndexes: #() ].
]

{ #category : #'accessing selection' }
FTTableMorph >> selectRowIndexes: anArray [
	self selectRowIndexes: anArray andMakeVisibleIf: true
]

{ #category : #'accessing selection' }
FTTableMorph >> selectRowIndexes: anArray andMakeVisibleIf: shouldEnsureVisibleSelection [
	| oldSelectedRowIndexes |
	anArray = selectedRowIndexes
		ifTrue: [ ^ self ].
	oldSelectedRowIndexes := selectedRowIndexes.
	self basicSelectRowIndexes: anArray.
	shouldEnsureVisibleSelection ifTrue: [ self ensureVisibleFirstSelection ].
	self refresh.
	self
		doAnnounce:
			((FTSelectionChanged from: oldSelectedRowIndexes to: selectedRowIndexes)
				fastTable: self;
				yourself)
]

{ #category : #'accessing selection' }
FTTableMorph >> selectedRowIndex [
	^ self selectedRowIndexes 
		ifNotEmpty: [ :indexes | indexes first ]
		ifEmpty: [ 0 ] 
]

{ #category : #'accessing selection' }
FTTableMorph >> selectedRowIndexes [
	^ selectedRowIndexes
]

{ #category : #'accessing colors' }
FTTableMorph >> selectionColor [
	^ selectionColor ifNil: [ self class defaultSelectionColor ]
]

{ #category : #'accessing colors' }
FTTableMorph >> selectionColor: aColor [
	selectionColor := aColor
]

{ #category : #private }
FTTableMorph >> selectionStrategy [
	^ selectionStrategy
]

{ #category : #accessing }
FTTableMorph >> setMultipleSelection: aBoolean [

	aBoolean
		ifTrue: [ self beMultipleSelection ]
		ifFalse: [ self beSingleSelection ]
]

{ #category : #accessing }
FTTableMorph >> showColumnHeaders [
	"Indicates table will show column headers. 
	 See #hideColumnHeaders"
	showColumnHeaders ifTrue: [ ^ self ].
	showColumnHeaders := true.
	self refresh
]

{ #category : #accessing }
FTTableMorph >> showFirstRowSelection [
	self hasSelection ifFalse: [ ^ self ].
	self moveShowIndexTo: self selectedRowIndex
]

{ #category : #private }
FTTableMorph >> showIndex [ 
	^ showIndex
]

{ #category : #menu }
FTTableMorph >> showMenuForPosition: aPoint [
	| tuple |
	
	tuple := self container rowAndColumnIndexContainingPoint: aPoint.
	self 
		showMenuForRowIndex: tuple first
		columnIndex: tuple second
]

{ #category : #menu }
FTTableMorph >> showMenuForRowIndex: rowIndex columnIndex: columnIndex [
	| menu |

	(rowIndex notNil and: [ (self isRowIndexSelected: rowIndex) not ]) ifTrue: [ 
		self selectRowIndex: rowIndex ].

	menu := self dataSource 
		menuColumn: (columnIndex ifNotNil: [self columns at: columnIndex])
		row: (rowIndex ifNil: [0]).
		
	(menu isNil or: [ menu isInWorld ]) ifTrue: [ ^ self ].
	
	menu popUpInWorld: self currentWorld

	
]

{ #category : #'drag and drop' }
FTTableMorph >> startDrag: event [

	| passengers transferMorph |

	event hand hasSubmorphs ifTrue: [^ self]. 
	self dragEnabled ifFalse: [^ self].
	"Here I ensure at least one element is selected "
	event hand anyButtonPressed ifFalse: [ ^self ].
	self hasSelection ifFalse: [ ^ self ].
			
	passengers := self selectedRowIndexes collect: [ :each | self dataSource passengerAt: each ].
	transferMorph := self dataSource transferFor: passengers from: self.
	transferMorph align: transferMorph draggedMorph topLeft with: event position.
	transferMorph dragTransferType: self dataSource dragTransferType.
	
	event hand grabMorph: transferMorph 
]

{ #category : #'event testing' }
FTTableMorph >> takesKeyboardFocus [
	^ self enabled
]

{ #category : #updating }
FTTableMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged.
	self refresh
]

{ #category : #updating }
FTTableMorph >> update: symbol [
	symbol == #refresh ifTrue: [ ^ self refresh ].
	^ super update: symbol
	
]

{ #category : #private }
FTTableMorph >> verticalScrollBar [
	^ verticalScrollBar
]

{ #category : #private }
FTTableMorph >> verticalScrollBarValue: aNumber [

	self hasDataSource ifFalse: [ ^ self ].
	
	showIndex := self verticalScrollBarValueToRowIndex: aNumber.
	
	self container changed
]

{ #category : #private }
FTTableMorph >> verticalScrollBarValueToRowIndex: aNumber [
	| startingIndex |
	startingIndex := self dataSource numberOfRows - self container calculateMinVisibleRows + 1.
	^ (startingIndex * aNumber) asInteger
]

{ #category : #private }
FTTableMorph >> verticalScrollBarWidth [
	self isVerticalScrollBarVisible ifFalse: [  ^ 0 ].
	^ self scrollBarThickness
]

{ #category : #accessing }
FTTableMorph >> visibleRowMorphAtIndex: index [
	^ container visibleRowMorphAtIndex: index
]

{ #category : #'drag and drop' }
FTTableMorph >> wantsDroppedMorph: aMorph event: event [
	aMorph isTransferable ifFalse: [ ^false ].
	^ self dataSource 
		wantsDropElements: aMorph passenger
		type: aMorph dragTransferType
		index: ((self container rowIndexContainingPoint: event position) ifNil: [ 0 ])
]
