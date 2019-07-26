"
A manager caring about a morphic based tree
"
Class {
	#name : #MorphTreeListManager,
	#superclass : #Object,
	#instVars : [
		'keystrokeActionSelector',
		'multipleSelection',
		'autoMultiSelection',
		'potentialDropMorph',
		'firstClickedMorph',
		'lastClickedMorph',
		'columnDropUnabled',
		'autoDeselection',
		'searchedElement',
		'lastKeystrokeTime',
		'lastKeystrokes',
		'lastSelection',
		'client',
		'isSelectionUpdateFromView',
		'isCheckList',
		'autoTargetMorph',
		'selectedMorphList',
		'removeOnlyLastSelected',
		'doubleClickSelector'
	],
	#category : #'Morphic-Widgets-Tree'
}

{ #category : #'selection change' }
MorphTreeListManager >> addAllToSelection: aCollection [
| selHolder |
	self searchedElement: nil.
	aCollection do: [:m | m 
		highlight;
		selected: true ].
	self selectionChanged.
	self selectionUpdateFromViewWhile: [
		selHolder := self newSelectionHolderWithNodePath: (aCollection last path collect: [:m | m complexContents]).
		self listModel selection: selHolder]

]

{ #category : #'selection change' }
MorphTreeListManager >> addToSelection: aMorph [
	aMorph selected ifTrue: [^false].
	self searchedElement: nil.
	aMorph highlight.
	aMorph selected: true.
	^ true
]

{ #category : #'selection change' }
MorphTreeListManager >> addToSelection: aMorph clickedTheCheckBox: checkBox [
	aMorph selected ifTrue: [^false].
	self searchedElement: nil.
	checkBox ifFalse: [ aMorph highlight ].
	aMorph selected: true.
	^ true
]

{ #category : #'client list accessing' }
MorphTreeListManager >> allNodeMorphs [
	^ client allNodeMorphs
]

{ #category : #'keyboard managing' }
MorphTreeListManager >> arrowEvent: event key: aChar [ 
	"Handle a keyboard navigation character. Answer true if handled, false
	if not."
	| newIndex targetMorph targetIndex multi |
	
	self allNodeMorphs ifEmpty: [ ^ false].
	self listModel okToChange ifFalse: [^ false].
	self listModel okToDiscardEdits ifFalse: [^ false].

	newIndex := nil.
	targetMorph := lastClickedMorph.
	"Give the oportunity to model to handle the keystroke event. 
	 This is an ugly HACK due to the lack of proper key handling... but I can live with it for now"
	targetMorph ifNotNil: [  
		(self listModel arrowEvent: event key: aChar target: targetMorph)
			ifTrue:  [ ^  true ]].

	targetIndex := targetMorph
		ifNil: [ 0 ]
		ifNotNil: [ targetMorph index ].
	multi := (event shiftPressed and: [self isMultiple]) or: [self autoMultiSelection].
	aChar = Character arrowDown
		ifTrue: [newIndex := targetIndex + 1].
	aChar = Character arrowUp
		ifTrue: [newIndex := targetIndex - 1 max: 1].
	aChar = Character home
		ifTrue: [newIndex := 1].
	aChar = Character end
		ifTrue: [newIndex := self allNodeMorphs size].
	aChar = Character pageUp
		ifTrue: [newIndex := targetIndex - self numSelectionsInView max: 1].
	aChar = Character pageDown
		ifTrue: [newIndex := targetIndex + self numSelectionsInView].
	aChar = Character arrowRight
		ifTrue: [
			targetMorph ifNil: [^ false].
			(targetMorph canExpand and: [targetMorph isExpanded not])
				ifTrue: [
					self toggleExpandedState: targetMorph.
					^ true]
				ifFalse: [ newIndex := targetIndex + 1 ] ].
	aChar = Character arrowLeft
		ifTrue: [
			targetMorph ifNil: [^ false].
			(targetMorph canExpand and: [targetMorph isExpanded])
				ifTrue: [
					self toggleExpandedState: targetMorph.
					^ true]
				ifFalse: [ | parent |
					parent := targetMorph parent.
					parent
						ifNil: [newIndex := targetIndex - 1 max: 1]
						ifNotNil: [
							self toggleExpandedState: parent.
							newIndex := parent index]]].
	newIndex notNil
		ifTrue: [
			self
				setSelectionIndexFromKeyboard: newIndex
				multiSelection: multi
				event: event.
			self selectionChanged.
			^ true].
	^ false
]

{ #category : #'selection change' }
MorphTreeListManager >> autoAction [
	^ firstClickedMorph
		ifNotNil: [
			firstClickedMorph selected
				ifTrue: [#addToSelection:]
				ifFalse: [#removeFromSelection:]]. 
	
]

{ #category : #accessing }
MorphTreeListManager >> autoDeselection [
	^ autoDeselection
		ifNil: [autoDeselection := false]
]

{ #category : #accessing }
MorphTreeListManager >> autoDeselection: aBoolean [

	autoDeselection := aBoolean
]

{ #category : #'selection change' }
MorphTreeListManager >> autoMultiSelect: aTargetMorph [
	autoTargetMorph := aTargetMorph.
	self autoAction
		ifNotNil: [:act |
			self
				from: firstClickedMorph
				to: aTargetMorph
				do: [:nd | self perform: act with: nd].
			self selectionUpdateFromViewWhile: [ | selHolder | 
				selHolder := self newSelectionHolderWithNodePath: nil.
				self listModel selection: selHolder].
			self selectionChanged.
			self scrollToShow: aTargetMorph contentBounds]
]

{ #category : #accessing }
MorphTreeListManager >> autoMultiSelection [
	^ autoMultiSelection ifNil: [autoMultiSelection := false]
]

{ #category : #accessing }
MorphTreeListManager >> autoMultiSelection: aBoolean [
	autoMultiSelection := aBoolean.
	aBoolean ifTrue: [self multipleSelection: true]
]

{ #category : #'keyboard managing' }
MorphTreeListManager >> basicKeyPressed: aChar [
    | nextSelection oldSelection milliSeconds slowKeyStroke nextSelectionNodeMorph |
   
    (aChar == Character space and: [ lastClickedMorph notNil ])
        ifTrue: [
            self
                selectMorph: lastClickedMorph
                multiple: (self isMultiple or: [self autoMultiSelection]).
            self selectionUpdateFromViewWhile: [ | selHolder |
                selHolder := self newSelectionHolderWithNodePath: lastClickedMorph complexContents path.
                self listModel selection: selHolder]].
       
    nextSelection := oldSelection := lastSelection.
    milliSeconds := Time millisecondClockValue.
    slowKeyStroke := milliSeconds - lastKeystrokeTime > 500.
    lastKeystrokeTime := milliSeconds.
   
    self searchedElement: nil.
   
    slowKeyStroke
        ifTrue: ["forget previous keystrokes and search in following elements"
            oldSelection := oldSelection + 1.
            lastKeystrokes := aChar asLowercase asString.]
        ifFalse: ["append quick keystrokes but don't move selection if it still matches"
            lastKeystrokes := lastKeystrokes , aChar asLowercase asString.].

    oldSelection isZero
        ifFalse:
            [ nextSelectionNodeMorph := self allNodeMorphs
                detect:
                    [ :a |
                    a index >= oldSelection and: [
                    "Get rid of blanks and style used in some lists"
                    a complexContents asString trimBoth asLowercase beginsWith: lastKeystrokes ]]
                ifNone: [     "roll over" oldSelection := 0 ] ].
               
    oldSelection isZero
        ifTrue:
            [ nextSelectionNodeMorph := self allNodeMorphs
                detect:
                    [ :a |
                    "Get rid of blanks and style used in some lists"
                    a complexContents asString trimBoth asLowercase beginsWith: lastKeystrokes ]
                ifNone: [ ^ self ] ].
           
    nextSelection := nextSelectionNodeMorph index.
    "No change if model is locked"
    self listModel okToChange ifFalse: [^ self].
    self searchedElement: nextSelectionNodeMorph.

    lastSelection := nextSelection.
    "change scrollbarvalue"
    self scrollToShow: nextSelectionNodeMorph contentBounds.
    self selectionChanged
]

{ #category : #'mouse managing' }
MorphTreeListManager >> clicked: event inTheCheckboxOf: aTargetMorph [
	| position |

	aTargetMorph ifNil: [ ^ false ].
	position := aTargetMorph point: event position from: client.
	^ self isCheckList and: [ (aTargetMorph checkClickableZone translateBy: 2 @ 0) containsPoint: position ]
]

{ #category : #accessing }
MorphTreeListManager >> client: aMorphList [
	client := aMorphList
]

{ #category : #accessing }
MorphTreeListManager >> columnDropUnabled [

	^ columnDropUnabled
]

{ #category : #accessing }
MorphTreeListManager >> columnDropUnabled: aBoolean [

	columnDropUnabled := aBoolean
]

{ #category : #'client list accessing' }
MorphTreeListManager >> commandOrCrontrolKeyPressed: anEvent [
	^ client commandOrCrontrolKeyPressed: anEvent
]

{ #category : #'selection change' }
MorphTreeListManager >> deselectAll [
	| selHolder |
	self allNodeMorphs isEmpty
		ifTrue: [^ self].
	self emptySelection.
	self selectionUpdateFromViewWhile: [
		selHolder := self newSelectionHolderWithNodePath: nil.
		self listModel selection: selHolder]


]

{ #category : #'mouse managing' }
MorphTreeListManager >> doubleClick: anEvent on: aMorph [
	self doubleClickSelector  ifNil: [^false].
	client model perform: self doubleClickSelector withEnoughArguments: { anEvent. aMorph }.
	^ true
]

{ #category : #accessing }
MorphTreeListManager >> doubleClickSelector [

	^ doubleClickSelector 
]

{ #category : #accessing }
MorphTreeListManager >> doubleClickSelector: aSelector [

	doubleClickSelector := aSelector
]

{ #category : #'selection change' }
MorphTreeListManager >> emptySelection [ 
	self selectedMorphList do: [:n | n unhighlight; setSelectedSilently: false].
	self selectedMorphList removeAll.
	self selectionChanged

]

{ #category : #accessing }
MorphTreeListManager >> firstClickedMorph [

	^ firstClickedMorph
]

{ #category : #accessing }
MorphTreeListManager >> firstClickedMorph: aNodeMorph [

	firstClickedMorph := aNodeMorph
]

{ #category : #'selection change' }
MorphTreeListManager >> from: aNodeMorph to: anotherNodeMorph do: action [
	| idx1 idx2 |
	idx1 := ((self getSelectionIndexOf: aNodeMorph) min: self allNodeMorphs size) max: 1.
	idx2 := ((self getSelectionIndexOf: anotherNodeMorph) min: self allNodeMorphs size) max: 1.
	(idx1 min: idx2) to: (idx1 max: idx2) do: [:idx | action value: (self allNodeMorphs at: idx)]

]

{ #category : #accessing }
MorphTreeListManager >> getSelectionIndexOf: aMorph [
	^ aMorph ifNil: [0] ifNotNil: [aMorph index]
]

{ #category : #initialization }
MorphTreeListManager >> initialize [
	super initialize.
	lastKeystrokeTime := 0.
	lastKeystrokes := ''.
	lastSelection := 0.
	removeOnlyLastSelected := false
]

{ #category : #accessing }
MorphTreeListManager >> isCheckList [
	^ isCheckList ifNil: [isCheckList := false]
]

{ #category : #accessing }
MorphTreeListManager >> isCheckList: aBoolean [
	isCheckList := aBoolean
]

{ #category : #accessing }
MorphTreeListManager >> isMultiple [	
	^ multipleSelection ifNil: [ multipleSelection := false ]
]

{ #category : #'selection change' }
MorphTreeListManager >> isSelectionUpdateFromView [
	^ isSelectionUpdateFromView ifNil: [isSelectionUpdateFromView := false] 
]

{ #category : #'keyboard managing' }
MorphTreeListManager >> keyStroke: anEvent [
	| char args |

	char := anEvent keyValue asCharacter. 

	(self arrowEvent: anEvent key: char)
		ifTrue: [ ^ true ].

	char = Character cr
		ifTrue: [ 
			self selectSearchedElement. 
			^ true ].
		
	anEvent anyModifierKeyPressed 
		ifFalse: [
			self basicKeyPressed: char.
			^ true].

	self keystrokeActionSelector 
		ifNil: [ ^ false].
		
	args := self keystrokeActionSelector numArgs.
	(args = 0 or: [args > 2]) 
		ifTrue: [^ self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol']
		ifFalse: [
			^ args = 1 
				ifTrue: [
					self listModel 
						perform: self keystrokeActionSelector 
						with: anEvent]
				ifFalse: [
					self listModel 
						perform: self keystrokeActionSelector
						with: anEvent 
						with: self]]
]

{ #category : #accessing }
MorphTreeListManager >> keystrokeActionSelector [

	^ keystrokeActionSelector
]

{ #category : #accessing }
MorphTreeListManager >> keystrokeActionSelector: aSelectorOrBlock [

	keystrokeActionSelector := aSelectorOrBlock
]

{ #category : #accessing }
MorphTreeListManager >> lastClickedMorph [

	^ lastClickedMorph
]

{ #category : #accessing }
MorphTreeListManager >> lastClickedMorph: aNodeMorph [

	lastClickedMorph ifNotNil: [ lastClickedMorph complexContents lastClicked: false ].
	lastClickedMorph := aNodeMorph.
	aNodeMorph ifNotNil: [ aNodeMorph complexContents lastClicked: true ].
]

{ #category : #accessing }
MorphTreeListManager >> lastKeystrokeTime [

	^ lastKeystrokeTime
]

{ #category : #accessing }
MorphTreeListManager >> lastKeystrokeTime: anObject [

	lastKeystrokeTime := anObject
]

{ #category : #accessing }
MorphTreeListManager >> lastKeystrokes [

	^ lastKeystrokes
]

{ #category : #accessing }
MorphTreeListManager >> lastKeystrokes: anObject [

	lastKeystrokes := anObject
]

{ #category : #accessing }
MorphTreeListManager >> lastSelection [

	^ lastSelection
]

{ #category : #accessing }
MorphTreeListManager >> lastSelection: anObject [

	lastSelection := anObject
]

{ #category : #accessing }
MorphTreeListManager >> listModel [
	^ client model
]

{ #category : #'mouse managing' }
MorphTreeListManager >> mouseDown: event on: aTargetMorph [
	"Changed to take keybaord focus."
	| clickedTheCheckbox |
	
	clickedTheCheckbox := self clicked: event inTheCheckboxOf: aTargetMorph.
	
	(self autoMultiSelection and: [ event shiftPressed not ])
		ifTrue: [ 
			firstClickedMorph := aTargetMorph.
			aTargetMorph selected
				ifTrue: [ 
					self removeFromSelection: aTargetMorph.
					(clickedTheCheckbox not and: [ self selectOnlyLastHighlighted and: [ aTargetMorph hasContentToShow ] ])
						ifTrue: [ 
							aTargetMorph = self lastClickedMorph
								ifFalse: [ self addToSelection: aTargetMorph ] ] ]
				ifFalse: [ 
					(clickedTheCheckbox not and: [ self selectOnlyLastHighlighted and: [ aTargetMorph hasContentToShow ] ])
						ifTrue: [ 
							aTargetMorph = self lastClickedMorph
								ifTrue: [ 
									self addToSelection: aTargetMorph.
									clickedTheCheckbox
										ifFalse: [ self lastClickedMorph: aTargetMorph ] ] ]
						ifFalse: [ 
							self addToSelection: aTargetMorph.
							clickedTheCheckbox
								ifFalse: [ self lastClickedMorph: aTargetMorph ] ] ] ].
	
	(clickedTheCheckbox not and: [ event shiftPressed not or: [ firstClickedMorph isNil ] ])
		ifTrue: [ firstClickedMorph := aTargetMorph ].
	
	aTargetMorph mouseDown: event
]

{ #category : #'mouse managing' }
MorphTreeListManager >> mouseMove: evt on: aTargetMorph [

	(aTargetMorph isNil or: [aTargetMorph highlightedForMouseDown not])
		ifTrue: [self allNodeMorphs do: [:m |
				m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
				aTargetMorph ifNotNil: [aTargetMorph highlightForMouseDown.
				(self autoMultiSelection and: [evt shiftPressed not])
					ifTrue: [
						self autoMultiSelect: aTargetMorph.
						self selectionChanged]]]
]

{ #category : #'mouse managing' }
MorphTreeListManager >> mouseUp: event on: aNodeMorph [ 
	| path cmdOrCtrl clickedTheCheckbox |
	"No change if model is locked" 
	
	clickedTheCheckbox := self clicked: event inTheCheckboxOf: aNodeMorph.
	self listModel okToChange ifFalse: [^self].
	self listModel okToDiscardEdits ifFalse: [^ self].

	cmdOrCtrl := self commandOrCrontrolKeyPressed: event.
	
	path := (event shiftPressed and: [self isMultiple])
				ifTrue: [self shiftSelectMorph: aNodeMorph]
				ifFalse: [
					self autoMultiSelection
						ifTrue: [self selectedMorphList ifEmpty: [] ifNotEmpty: [:l | l last path collect: [:p | p complexContents]]]
						ifFalse: [self selectMorph: aNodeMorph multiple: ((cmdOrCtrl and: [self isMultiple]) or: [self autoMultiSelection]) clickedTheCheckBox: clickedTheCheckbox ]].
	self selectionUpdateFromViewWhile: [ | selHolder | 
		selHolder := self newSelectionHolderWithNodePath: path.
		self listModel selection: selHolder].
	
	clickedTheCheckbox ifFalse: [ self lastClickedMorph: aNodeMorph ].
	self selectionChanged. 

]

{ #category : #accessing }
MorphTreeListManager >> multipleSelection [

	^ multipleSelection ifNil: [multipleSelection := false]
]

{ #category : #accessing }
MorphTreeListManager >> multipleSelection: anObject [

	multipleSelection := anObject
]

{ #category : #'selection change' }
MorphTreeListManager >> newSelectionHolderWithNodePath: aNodePath [ 
	^ self isMultiple
		ifTrue: [MorphTreeMorphMultipleSelection new
				selectedNodePathList: (self selectedMorphList
						collect: [:s | s path	collect: [:m | m complexContents]])] 
		ifFalse: [MorphTreeMorphSingleSelection new selectedNodePath: aNodePath]
]

{ #category : #accessing }
MorphTreeListManager >> nodeMorphsWithAllNodeItems: aNodeItemList [
	| result |

	result := OrderedCollection new.
	
	self allNodeMorphs do: [:m | 
		aNodeItemList do: [ :sel | (m expandPath: sel) ] ].
	
	self allNodeMorphs do: [:m | 
		aNodeItemList do: [ :sel | (m matchPath: sel)
			ifNotNil: [:col | result addAll: col ] ] ].

	^ result flattened
]

{ #category : #'selection change' }
MorphTreeListManager >> noteRemovalOfAll: aMorphList [
	"When noting the removal of items, do not change their model status: the model will be in charge of it (i.e. either updates from the model or expand/collapse things)."

	aMorphList
		do: [ :each | 
			self selectedMorphList remove: each ifAbsent: [  ].
			lastClickedMorph == each
				ifTrue: [ lastClickedMorph := nil ] ]
]

{ #category : #'client list accessing' }
MorphTreeListManager >> numSelectionsInView [
	^ client numSelectionsInView
]

{ #category : #accessing }
MorphTreeListManager >> potentialDropMorph [

	^ potentialDropMorph
]

{ #category : #accessing }
MorphTreeListManager >> potentialDropMorph: anObject [

	potentialDropMorph := anObject
]

{ #category : #'selection change' }
MorphTreeListManager >> removeFromSelection: aMorph [
	aMorph selected ifFalse: [^false].
	aMorph selected: false.
	aMorph unhighlight.
	self selectionUpdateFromViewWhile: [ | selHolder | 
		selHolder := self newSelectionHolderWithNodePath: nil.
		self listModel selection: selHolder].
	^ true

]

{ #category : #accessing }
MorphTreeListManager >> removeOnlyLastSelected: aBoolean [

	removeOnlyLastSelected := aBoolean
]

{ #category : #'client list accessing' }
MorphTreeListManager >> scrollToShow: aRectangle [
	client scrollToShow: aRectangle
]

{ #category : #accessing }
MorphTreeListManager >> searchedElement [

	^ searchedElement
]

{ #category : #accessing }
MorphTreeListManager >> searchedElement: anObject [

	searchedElement := anObject
]

{ #category : #'selection accessing' }
MorphTreeListManager >> secondSelection: anItem [

 	| nodeMorph |
	nodeMorph := self allNodeMorphs detect: [ :e | e complexContents item == anItem ].
	self searchedElement: nodeMorph.
	client changed.
]

{ #category : #'selection change' }
MorphTreeListManager >> selectAll [
	self allNodeMorphs isEmpty
		ifTrue: [^ self].
	self isMultiple
		ifFalse: [^ self]. 
	self addAllToSelection: self allNodeMorphs
]

{ #category : #'selection change' }
MorphTreeListManager >> selectMoreAtBottom [
	| last |
	autoTargetMorph ifNil: [^self].
	last := autoTargetMorph index.
	last < self allNodeMorphs size
		ifTrue: [
			self autoMultiSelect: (self allNodeMorphs at: last + 1).
			self selectionChanged]
]

{ #category : #'selection change' }
MorphTreeListManager >> selectMoreAtTop [
	| first |
	autoTargetMorph ifNil: [^self].
	first := autoTargetMorph index.
	first > 1
		ifTrue: [
			self autoMultiSelect: (self allNodeMorphs at: first - 1).
			self selectionChanged]
]

{ #category : #'selection change' }
MorphTreeListManager >> selectMorph: aNodeMorph multiple: withMultipleSelection [

	| path mult |

	self lastClickedMorph: aNodeMorph.
	aNodeMorph
		ifNil: [ self emptySelection.
			^ nil
			].
	aNodeMorph selected
		ifTrue: [ withMultipleSelection
				ifTrue: [ self removeFromSelection: aNodeMorph ]
				ifFalse: [ mult := self selectedMorphList size > 1.
					path := aNodeMorph path collect: [ :m | m complexContents ].
					( self autoDeselection or: [ mult ] )
						ifTrue: [ self emptySelection.
							mult
								ifTrue: [ self addToSelection: aNodeMorph ]
								ifFalse: [ path := nil ]
							]
					]
			]
		ifFalse: [ withMultipleSelection
				ifFalse: [ self emptySelection ].
			self addToSelection: aNodeMorph.
			path := aNodeMorph path collect: [ :m | m complexContents ]
			].
	self selectionChanged.
	^ path
]

{ #category : #'selection change' }
MorphTreeListManager >> selectMorph: aNodeMorph multiple: withMultipleSelection clickedTheCheckBox: checkBox [

	| path mult |

	checkBox
		ifFalse: [ self lastClickedMorph: aNodeMorph ].
	aNodeMorph
		ifNil: [ self emptySelection.
			^ nil
			].
	aNodeMorph selected
		ifTrue: [ withMultipleSelection
				ifTrue: [ self removeFromSelection: aNodeMorph ]
				ifFalse: [ mult := self selectedMorphList size > 1.
					path := aNodeMorph path collect: [ :m | m complexContents ].
					( self autoDeselection or: [ mult ] )
						ifTrue: [ self emptySelection.
							mult
								ifTrue: [ self addToSelection: aNodeMorph ]
								ifFalse: [ path := nil ]
							]
					]
			]
		ifFalse: [ withMultipleSelection
				ifFalse: [ self emptySelection ].
			self addToSelection: aNodeMorph clickedTheCheckBox: checkBox.
			path := aNodeMorph path collect: [ :m | m complexContents ]
			].
	self selectionChanged.
	^ path
]

{ #category : #accessing }
MorphTreeListManager >> selectOnlyLastHighlighted [
	^ removeOnlyLastSelected
]

{ #category : #private }
MorphTreeListManager >> selectSearchedElement [

	self searchedElement 
		ifNotNil: [ :selectedMorph | 
			self setSelectedMorph: selectedMorph.
			self searchedElement: nil ]
]

{ #category : #'selection change' }
MorphTreeListManager >> selectedItems: aNodeItemCollection [

	self listModel okToDiscardEdits ifFalse: [^ self].
	self emptySelection.
	(self nodeMorphsWithAllNodeItems: aNodeItemCollection) ifNotEmpty:[:selection |
	self addAllToSelection: selection].
	lastClickedMorph
		ifNil: [lastClickedMorph := self selectedMorphList
						ifEmpty: []
						ifNotEmpty: [self selectedMorphList last]]
]

{ #category : #'selection accessing' }
MorphTreeListManager >> selectedMorph [
	^ self selectedMorphList
		ifNotEmpty: [ :l | l last]
 		ifEmpty: []

]

{ #category : #'selection accessing' }
MorphTreeListManager >> selectedMorphList [
	^ selectedMorphList ifNil: [selectedMorphList := LinkedList new]
]

{ #category : #'selection change' }
MorphTreeListManager >> selectionChanged [
	client selectionChanged

]

{ #category : #'selection change' }
MorphTreeListManager >> selectionUpdateFromViewWhile: aBlock [
	| prev |
	prev := self isSelectionUpdateFromView.
	isSelectionUpdateFromView := true.
	aBlock ensure: [isSelectionUpdateFromView := prev]

]

{ #category : #'selection change' }
MorphTreeListManager >> setSelectedMorph: aMorph [
	| path |
	path := aMorph ifNotNil: [ aMorph path collect: [ :m | m complexContents ] ].
	self lastClickedMorph: aMorph.
	self emptySelection.
	aMorph 
		ifNotNil: [ self addToSelection: lastClickedMorph ].
	self selectionUpdateFromViewWhile: [ 
			self listModel selection: (self newSelectionHolderWithNodePath: path) ]
]

{ #category : #'keyboard managing' }
MorphTreeListManager >> setSelectionIndexFromKeyboard: index multiSelection: multiSelect event: anEvent [
	"Called internally to select the index-th item."
	| targetMorph |
	index ifNil: [^ self].
	index > self allNodeMorphs size ifTrue: [^self].
	targetMorph := index = 0 ifTrue: [nil] ifFalse: [self allNodeMorphs at: index].
	lastClickedMorph ifNotNil: [lastClickedMorph highlightForMouseDown: false].
	self isCheckList
		ifTrue: [
			(multiSelect and: [anEvent shiftPressed])
				ifTrue: [self autoMultiSelect: targetMorph]]
		ifFalse: [
			(multiSelect and: [anEvent shiftPressed])
				ifTrue: [self autoMultiSelect: targetMorph]
				ifFalse: [self setSelectedMorph: targetMorph]].
	self lastClickedMorph: (firstClickedMorph := targetMorph).
	anEvent shiftPressed
		ifTrue: [lastClickedMorph highlightForMouseDown: true].

	self selectionChanged.
	self scrollToShow: targetMorph contentBounds
	

]

{ #category : #'selection change' }
MorphTreeListManager >> shiftSelectMorph: aNodeMorph [ 
	| m |
	m := aNodeMorph ifNil: [self allNodeMorphs last].
	self emptySelection.
	self searchedElement: nil.
	self 
		from: firstClickedMorph 
		to: m 
		do: [:nd | self addToSelection: nd]. 
	self lastClickedMorph: m.
	^ m path collect: [:p | p complexContents]
]

{ #category : #'selection change' }
MorphTreeListManager >> silentlySetSelectedMorph: aMorph [
	| path |
	
	path := aMorph ifNotNil: [ aMorph path collect: [ :m | m complexContents ] ].
	self isMultiple ifFalse: [ self emptySelection ].
	
	aMorph ifNotNil: [ self addToSelection: aMorph ].
	
	self selectionUpdateFromViewWhile: [ 
			self listModel selection: (self newSelectionHolderWithNodePath: path) ]
]

{ #category : #'selection change' }
MorphTreeListManager >> toggleExpandedState: aMorph [
	aMorph toggleExpandedState.
	client innerWidgetChanged


]

{ #category : #'selection change' }
MorphTreeListManager >> updateLastClickedMorph [
	"When the contents have been updated, ensure that lastClickedMorph refer to a correct morph."

	lastClickedMorph
		ifNil: [ lastClickedMorph := self selectedMorphList ifEmpty: [  ] ifNotEmpty: [ self selectedMorphList last ] ]
		ifNotNil: [ 
			self allNodeMorphs
				do: [ :each | 
					each complexContents withoutListWrapper = lastClickedMorph complexContents withoutListWrapper
						ifTrue: [ lastClickedMorph := each ] ] ]
]

{ #category : #'selection change' }
MorphTreeListManager >> updateSelectionFromModel [
	(self listModel selection) 
		ifNotNil: [:selHolder | 	
				self isSelectionUpdateFromView
					ifTrue: [
						self listModel selectionChanged.
						self selectionChanged]
					ifFalse: [selHolder updateView: client forModel: self listModel]].

]
