"
I draw the node part of a tree.
"
Class {
	#name : #MorphTreeNodeMorph,
	#superclass : #Morph,
	#instVars : [
		'parent',
		'index',
		'indentLevel',
		'isExpanded',
		'complexContents',
		'firstChild',
		'container',
		'nextSibling',
		'controls',
		'lineColor',
		'selected'
	],
	#category : #'Morphic-Widgets-Tree'
}

{ #category : #'drag and drop' }
MorphTreeNodeMorph >> acceptDroppingMorph: toDrop event: evt [
	complexContents acceptDroppingObject: toDrop complexContents.
	toDrop delete.
	self highlightForDrop: false.
]

{ #category : #updating }
MorphTreeNodeMorph >> addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems [
	firstChild 
		ifNotNil: [
			firstChild withSiblingsDo: [ :aNode | aNode delete].
			firstChild := nil].
	complexContents hasContents ifFalse: [^self].
	firstChild := hostList 
		addMorphsTo: morphList
		from: complexContents contents
		withExpandedItems: expandedItems
		atLevel: indentLevel + 1.

]

{ #category : #updating }
MorphTreeNodeMorph >> adoptPaneColor: aColor [
]

{ #category : #announcements }
MorphTreeNodeMorph >> announceDeleted [

	complexContents ifNotNil: [ complexContents removeDependent: self ].
	
	super announceDeleted.
]

{ #category : #accessing }
MorphTreeNodeMorph >> balloonText [

	^complexContents balloonText ifNil: [super balloonText]
]

{ #category : #accessing }
MorphTreeNodeMorph >> boundsForBalloon [

	"some morphs have bounds that are way too big"
	container ifNil: [^super boundsForBalloon].
	^self boundsInWorld intersect: container boundsInWorld ifNone: [ self boundsInWorld ]
]

{ #category : #initialization }
MorphTreeNodeMorph >> buildRowMorph [
	| rowControls colAndControls |
	controls := OrderedCollection new.
	colAndControls := container columns 
		collect: [:col | | v |
			v := col rowMorphFor: complexContents.
			controls add: v.
			col -> v].
	rowControls := OrderedCollection new.
	colAndControls
		do: [:ctrl | | col morph |
			col := ctrl key.
			morph := ctrl value.
			morph clipSubmorphs: true.
			morph vResizing: #shrinkWrap.  
			rowControls add: morph.
			(morph = controls last and: [container lastColumnUnbounded]) ifFalse: [morph hResizing: #rigid].
			(col resizable not and: [col shrinkWrap])
				ifTrue: [col currentWidth < morph width 
						ifTrue: [col forceWidthTo: morph width]]].

	self addAllMorphs: rowControls.
	self layoutChanged
	

]

{ #category : #testing }
MorphTreeNodeMorph >> canExpand [
	^complexContents 
		ifNotNil: [ complexContents hasContents ]
		ifNil: [ false ]
]

{ #category : #updating }
MorphTreeNodeMorph >> changed [
	"Need to invalidate the selection frame."
	container
		ifNil: [super changed] 
		ifNotNil: [container invalidRect: self selectionFrame]
]

{ #category : #'mouse events' }
MorphTreeNodeMorph >> checkClickableZone [
	| topLeft icon |
	
	topLeft := self computeCheckTopLeft.
	icon := self retrieveCheckIcon.
	
	^ topLeft corner: icon extent + topLeft
]

{ #category : #accessing }
MorphTreeNodeMorph >> checkGap [
	^ 2
]

{ #category : #accessing }
MorphTreeNodeMorph >> checkRectangle [
	| tr |
	tr := self toggleRectangle translateBy: (3 @ 0).
	^ self mustTakeIntoAccountToggleSpace 
		ifTrue: [(tr topRight + (self checkGap @ 0)) corner:  tr bottomRight + ((self checkGap + self checkWidth) @ 0)]
		ifFalse: [tr]

]

{ #category : #accessing }
MorphTreeNodeMorph >> checkWidth [
	^ 10
]

{ #category : #accessing }
MorphTreeNodeMorph >> children [
	| children |
	children := OrderedCollection new.
	self childrenDo: [:each | children add: each].
	^children
]

{ #category : #updating }
MorphTreeNodeMorph >> childrenDo: aBlock [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aBlock value: aNode].
	]
]

{ #category : #updating }
MorphTreeNodeMorph >> closeItemPath: anArray [
	"Close a path based on wrapper item equivalence."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper = anArray first)
						ifTrue: [found := each]]].
	found
		ifNotNil: [(found isExpanded and: [anArray size = 1])
				ifTrue: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild closeItemPath: anArray allButFirst]].
	^container setSelectedMorph: nil
]

{ #category : #'expanding-collapsing' }
MorphTreeNodeMorph >> collapseNodePath: anArray [
	"Close a path based on node."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first)
						ifTrue: [found := each]]].
	found
		ifNotNil: [(found isExpanded and: [anArray size = 1])
				ifTrue: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild collapseNodePath: anArray allButFirst]].
	^container setSelectedMorph: nil
]

{ #category : #accessing }
MorphTreeNodeMorph >> color [
	^ complexContents color ifNil: [self index ifNotNil: [container rowColors at: ((self index \\ 2) + 1)]]
]

{ #category : #accessing }
MorphTreeNodeMorph >> columnMorphAt: anIndex [
	^ controls at: anIndex 
]

{ #category : #accessing }
MorphTreeNodeMorph >> complexContents [

	^complexContents
]

{ #category : #private }
MorphTreeNodeMorph >> computeCheckTopLeft [
	| center offset |
	
	center := self checkRectangle center.
	offset := (self checkWidth / 2.0) truncated.
	^ (center x - offset) @ (center y - offset - 1)
]

{ #category : #accessing }
MorphTreeNodeMorph >> controls [
	^ controls
]

{ #category : #updating }
MorphTreeNodeMorph >> delete [

	parent := nil.
	complexContents := nil.
	firstChild := nil.
	container := nil.
	nextSibling := nil.
	controls := nil.
	super delete.

]

{ #category : #drawing }
MorphTreeNodeMorph >> drawCheckOn: aCanvas [
	| topLeft icon |
	
	topLeft := self computeCheckTopLeft.
	icon := self retrieveCheckIcon.
	
	aCanvas
		drawImage: icon
		at: topLeft
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawLineToggleToTextOn: aCanvas hasToggle: hasToggle [
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge.
	Only draw now if no toggle."

	| myBounds myCenter hLineY hLineLeft myTheme ldelta |
	self isSoleItem ifTrue: [ ^self ].
	self hasToggle ifTrue: [^self].
	myBounds := self toggleRectangle.
	myCenter := myBounds center.
	hLineY := myCenter y - 1.
	ldelta := container treeLineWidth // 2.
	hLineLeft := myCenter x - ldelta.
	"Draw line from toggle to text. Use optimised form since vertical."
	myTheme := self theme.
	aCanvas
		frameRectangle: (hLineLeft @ (hLineY ) corner: myBounds right + 3  + ldelta @ (hLineY + container treeLineWidth ))
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: (self parent ifNil: [self lineColor] ifNotNil: [self parent lineColor]))
		dashes: self treeLineDashes
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawLinesOn: aCanvas [ 
	| hasToggle |
	hasToggle := self hasToggle.
	"Draw line from toggle to text"
	self drawLineToggleToTextOn: aCanvas hasToggle: hasToggle.

	"Draw the line from my toggle to the nextSibling's toggle"
	self nextSibling ifNotNil: [ self drawLinesToNextSiblingOn: aCanvas hasToggle: hasToggle ].

	"If I have children and am expanded, draw a line to my first child"
	(self firstChild notNil and: [ self isExpanded ])
		ifTrue: [ self drawLinesToFirstChildOn: aCanvas]
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawLinesToFirstChildOn: aCanvas [
	"Draw line from me to first child.
	Don't bother if the first child has a toggle.."

	| vLineX vLineTop vLineBottom childBounds childCenter myTheme ldelta |
	self firstChild hasToggle ifTrue: [^self].
	childBounds := self firstChild toggleRectangle.
	childCenter := childBounds center.
	vLineX := childCenter x.
	vLineTop := bounds bottom.
	ldelta := container treeLineWidth // 2.
	self firstChild hasToggle
		ifTrue: [vLineBottom := childCenter y - (childBounds height // 2) + ldelta]
		ifFalse: [vLineBottom := childCenter y - 2].
	myTheme := self theme.
	aCanvas
		frameRectangle: (vLineX - ldelta @ vLineTop corner: (vLineX + ldelta + (container treeLineWidth \\ 2)) @ vLineBottom)
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: self lineColor)
		dashes: self treeLineDashes
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawLinesToNextSiblingOn: aCanvas hasToggle: hasToggle [
	"Draw line from me to next sibling"
	
	| myBounds nextSibBounds vLineX myCenter vLineTop vLineBottom myTheme ldelta gap |
	myBounds := self toggleRectangle.
	nextSibBounds := self nextSibling toggleRectangle.
	myCenter := myBounds center.
	vLineX := myCenter x.
	gap := (container notExpandedForm extent y // 2) + 1.
	vLineTop := myCenter y + (self hasToggle ifTrue: [gap] ifFalse: [0]).
	vLineBottom := nextSibBounds center y - (self nextSibling hasToggle ifTrue: [gap] ifFalse: [0]).
	"Draw line from me to next sibling"
	myTheme := self theme.
	ldelta := container treeLineWidth // 2.
	aCanvas
		frameRectangle: (vLineX - ldelta @ vLineTop corner: vLineX + ldelta + (container treeLineWidth \\ 2) @ vLineBottom)
		width: container treeLineWidth
		colors: (myTheme treeLineColorsFrom: self lineColor)
		dashes: self treeLineDashes
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawMouseDownHighlightOn: aCanvas [
	"Draw with a dotted border."
	
	self highlightedForMouseDown 
		ifTrue: [
			container ifNil: [^super drawMouseDownHighlightOn: aCanvas].
			aCanvas 
				frameRectangle: self bounds
				width: 1
				colors: {container mouseDownHighlightColor. Color transparent}
				dashes: #(1 1)]
]

{ #category : #drawing }
MorphTreeNodeMorph >> drawOn: aCanvas [
	"Note that selection is rendered from the container transformMorph (see MorphTreeTransformMorph)" 
	
	container withHLines 
		ifTrue: [
			aCanvas 
				frameRectangle: self selectionFrame
				width: 1
				colors: {Color veryLightGray. Color transparent}
				 dashes: #(1 2)].

	self hasToggle 
		ifTrue: [self drawToggleOn: aCanvas in: self toggleRectangle]. 
		
	container listManager isCheckList
		ifTrue: [self drawCheckOn: aCanvas ].
	

		
		
		
		
		
		

]

{ #category : #drawing }
MorphTreeNodeMorph >> drawToggleOn: aCanvas in: aRectangle [

	| aForm centeringOffset |
	aForm := self toggleImageForm.
	centeringOffset := ((aRectangle height - aForm extent y) / 2.0) truncated.
	^aCanvas 
		translucentImage: aForm 
		at: (aRectangle topLeft translateBy: 0 @ centeringOffset).

]

{ #category : #updating }
MorphTreeNodeMorph >> expand [
	| c newChildren  |
	isExpanded := true.
	(c := complexContents contents) isEmpty ifTrue: [^self changed].
	newChildren := container addMorphsAfter: self fromCollection: c.
	firstChild := newChildren first.
	self updateChildren

	

]

{ #category : #'expanding-collapsing' }
MorphTreeNodeMorph >> expandItemPath: anArray [
	"Open a path."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild expandItemPath: anArray allButFirst]].
	^container setSelectedMorph: nil
]

{ #category : #testing }
MorphTreeNodeMorph >> expandPath: anAssociation [

	anAssociation ifNil: [ ^ false ].
	^ anAssociation treeNodeHead = self complexContents withoutListWrapper
		ifFalse: [ false ]
		ifTrue: [
			anAssociation treeNodeTail ifNil: [ ^ true ].
			(self isExpanded not and: [ self canExpand ]) ifTrue: [
				self toggleExpandedState.
				container innerWidgetChanged ].
			self children anySatisfy: [:child | child expandPath: anAssociation treeNodeTail ]]
]

{ #category : #accessing }
MorphTreeNodeMorph >> firstChild [

	^firstChild
]

{ #category : #layout }
MorphTreeNodeMorph >> fullBounds [
	fullBounds ifNotNil: [^ fullBounds].
	^ submorphs 
		ifEmpty: [bounds]
		ifNotEmpty: [
			self doLayoutIn: self layoutBounds.
			fullBounds]
]

{ #category : #'mouse events' }
MorphTreeNodeMorph >> handleMouseUp: anEvent [ 
	container enabled ifFalse: [ ^ false ].
	(container commandOrCrontrolKeyPressed: anEvent) 
		ifTrue: [^ container listManager mouseUp: anEvent on: self].
	^ super handleMouseUp: anEvent
]

{ #category : #testing }
MorphTreeNodeMorph >> hasContentToShow [

	^ self complexContents hasContentToShow
]

{ #category : #testing }
MorphTreeNodeMorph >> hasIcon [
	"Answer whether the receiver has an icon."
	^ container hasIconBlock or: [self complexContents icon notNil]
]

{ #category : #testing }
MorphTreeNodeMorph >> hasToggle [
	^ self canExpand
]

{ #category : #updating }
MorphTreeNodeMorph >> highlight [
	self
		allMorphsDo: [:m | (m isKindOf: StringMorph)
				ifTrue: [m setProperty: #originalColor toValue: m color.
					m color: self theme selectionTextColor]].
	complexContents highlightingColor 
		ifNotNil: [:c |
			self setProperty: #originalColor toValue: color.
			self color: c]
]

{ #category : #updating }
MorphTreeNodeMorph >> highlightForMouseDown: aBoolean [
	aBoolean 
		ifTrue: [self setProperty: #highlightedForMouseDown toValue: aBoolean]
		ifFalse: [self removeProperty: #highlightedForMouseDown].
	self changed
]

{ #category : #accessing }
MorphTreeNodeMorph >> icon [
	"answer the receiver's icon"
	
	^ container iconBlock value: self complexContents.
	
]

{ #category : #testing }
MorphTreeNodeMorph >> inToggleArea: aPoint [

	^self sensitiveToggleRectangle containsPoint: aPoint
]

{ #category : #accessing }
MorphTreeNodeMorph >> indentGap [
	^ container indentGap * indentLevel
]

{ #category : #accessing }
MorphTreeNodeMorph >> indentLevel [

	^indentLevel
]

{ #category : #accessing }
MorphTreeNodeMorph >> index [
	^ index 
]

{ #category : #accessing }
MorphTreeNodeMorph >> index: anInteger [
	index := anInteger
]

{ #category : #initialization }
MorphTreeNodeMorph >> initRow [
	self buildRowMorph.
	self layoutChanged

]

{ #category : #initialization }
MorphTreeNodeMorph >> initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel [
	"Make sure we properly register the delete event on the model."

	container := hostList.
	self cellInset: container resizerWidth @ 0.
	
	complexContents := anObject.
	complexContents addDependent: self.

	isExpanded := complexContents isExpanded.
	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [ priorMorph nextSibling: self ].
	indentLevel := newLevel.
	self setBalloonText: complexContents helpText.
	self initRow.
	complexContents selected
		ifTrue: [ self selectedWithoutNotifyingComplexContents: true ]
]

{ #category : #initialization }
MorphTreeNodeMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	self
		layoutPolicy: TableLayout new;
		cellPositioning: #leftCenter;
		listDirection: #leftToRight;
		cellSpacing: #none;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		color: Color transparent.



]

{ #category : #'change reporting' }
MorphTreeNodeMorph >> invalidRect: aRectangle [
	
]

{ #category : #testing }
MorphTreeNodeMorph >> isExpanded [

	^isExpanded
]

{ #category : #accessing }
MorphTreeNodeMorph >> isExpanded: aBoolean [

	isExpanded := aBoolean.
	self complexContents isExpanded: aBoolean
]

{ #category : #testing }
MorphTreeNodeMorph >> isFirstItem [
	^owner submorphs first == self
]

{ #category : #testing }
MorphTreeNodeMorph >> isPartialMatch [
	
	^ self complexContents isPartialMatch
]

{ #category : #testing }
MorphTreeNodeMorph >> isSelected [
	"^ container selectedMorphList includes: self"
	^ self selected
]

{ #category : #testing }
MorphTreeNodeMorph >> isSoleItem [
	^self isFirstItem and: [ owner submorphs size = 1 ]
]

{ #category : #accessing }
MorphTreeNodeMorph >> lastChild [
	"Answer the last child."

	|c|
	c := self firstChild ifNil: [^nil].
	[c nextSibling isNil] whileFalse: [c := c nextSibling].
	^c
]

{ #category : #layout }
MorphTreeNodeMorph >> layoutBounds [
	"Return the bounds for laying out children of the receiver"

	| lb left right |
	
	lb := super layoutBounds.
	container ifNil: [ ^ lb ].
	left := (lb left + self spacerWidth).
	right := (lb right max: left + self spacerWidth).
	"Make sure that left and right have changed simultaneously, avoiding creating degenerate rectangle"
	^ Rectangle left: left right: right top: lb top bottom: lb bottom

]

{ #category : #accessing }
MorphTreeNodeMorph >> lineColor [
	"Answer a good color to use for drawing the lines that connect members of the hierarchy view.
	Used the cached color, or derive it if necessary by finding the receiver or 
	the first owner (up to my root) that is not transparent, then picking a contrasting color.
	Fall back to black if all my owners are transparent."

	^ lineColor 
		ifNil: [lineColor := container lineColorForNode: self complexContents]
	

]

{ #category : #testing }
MorphTreeNodeMorph >> matchPath: anAssociation [

	anAssociation ifNil: [ ^ nil ].
	^ anAssociation treeNodeHead = self complexContents withoutListWrapper
		ifFalse: [ nil ]
		ifTrue: [ | matchingChildren |
			anAssociation treeNodeTail ifNil: [ ^ { self } ].
			matchingChildren := self children collect: [:child | child matchPath: anAssociation treeNodeTail ].
			^ matchingChildren select: [ :e | e notNil ] ]
]

{ #category : #accessing }
MorphTreeNodeMorph >> minWidth [
	"Fixed to work such that guessed width is unnecessary in
	#adjustSubmorphPositions."
	| gap |
	
	gap := container ifNil: [ 0 ] ifNotNil: [ self indentGap ].
	^ gap  max: super minWidth
]

{ #category : #'mouse events' }
MorphTreeNodeMorph >> mouseDown: event [
	container enabled ifFalse: [ ^self ].
	complexContents mouseDown: event
]

{ #category : #testing }
MorphTreeNodeMorph >> mustTakeIntoAccountCheckSpace [
	^container listManager isCheckList
]

{ #category : #testing }
MorphTreeNodeMorph >> mustTakeIntoAccountToggleSpace [
	^ indentLevel > 0 or: [	container hasToggleAtRoot]
]

{ #category : #accessing }
MorphTreeNodeMorph >> nextSibling [

	^nextSibling
]

{ #category : #accessing }
MorphTreeNodeMorph >> nextSibling: anotherMorph [

	nextSibling := anotherMorph
]

{ #category : #updating }
MorphTreeNodeMorph >> openItemPath: anArray [
	"Open a path based on wrapper item equivalence. Generally more specific
	than #openPath: (string based)."
	
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents withoutListWrapper = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState].
			found changed.
			anArray size = 1
				ifTrue: [^ container listManager setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild openItemPath: anArray allButFirst]].
	^self
]

{ #category : #'expanding-collapsing' }
MorphTreeNodeMorph >> openNodePath: anArray [ 
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild openNodePath: anArray allButFirst]].
	^ container setSelectedMorph: nil
]

{ #category : #updating }
MorphTreeNodeMorph >> openPath: anArray [ 
	| found |
	anArray isEmpty
		ifTrue: [^ container listManager setSelectedMorph: nil].
	found := nil.
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents asString = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNil: ["try again with no case sensitivity"
			self
				withSiblingsDo: [:each | found
						ifNil: [(each complexContents asString sameAs: anArray first)
								ifTrue: [found := each]]]].
	found
		ifNotNil: [found isExpanded
				ifFalse: [found toggleExpandedState.
					container adjustSubmorphPositions].
			found changed.
			anArray size = 1
				ifTrue: [^ container setSelectedMorph: found].
			^ found firstChild
				ifNil: [container setSelectedMorph: nil]
				ifNotNil: [found firstChild openPath: anArray allButFirst]].
	^ container setSelectedMorph: nil
]

{ #category : #accessing }
MorphTreeNodeMorph >> outerBounds [
	"Return the 'outer' bounds of the receiver, e.g., the bounds that need to be invalidated when the receiver changes."
	
	^ self bounds
]

{ #category : #accessing }
MorphTreeNodeMorph >> parent [
	^ parent
]

{ #category : #accessing }
MorphTreeNodeMorph >> parent: aNodeMorph [
	parent := aNodeMorph
]

{ #category : #accessing }
MorphTreeNodeMorph >> path [
	^ parent 
		ifNil: [OrderedCollection with: self]
		ifNotNil: [(parent path) add: self; yourself]
]

{ #category : #printing }
MorphTreeNodeMorph >> printOn: aStream [
	aStream nextPutAll: 'NodeMorph('.
	complexContents printOn: aStream.
	aStream nextPut: $)
]

{ #category : #updating }
MorphTreeNodeMorph >> recursiveAddTo: aCollection [

	firstChild ifNotNil: [firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection]].
	aCollection add: self.

	
]

{ #category : #updating }
MorphTreeNodeMorph >> recursiveDelete [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveDelete].
	].
	self delete
	
]

{ #category : #private }
MorphTreeNodeMorph >> retrieveCheckIcon [
	^ self selected
		ifTrue: [ self iconNamed: #checkedBoxIcon ]
		ifFalse: [ self isPartialMatch
				ifTrue: [ self iconNamed: #partialCheckedBoxIcon ]
				ifFalse: [ self iconNamed: #uncheckedBoxIcon ] ]
]

{ #category : #accessing }
MorphTreeNodeMorph >> rowMorphAt: anIndex [
	^ self submorphs seconds submorphs at: anIndex
]

{ #category : #updating }
MorphTreeNodeMorph >> selectNodePath: anArray [
	"select a node from a path based on wrapper node equivalence"
	
	| found |
	anArray isNil ifTrue: [^ self].
	anArray isEmpty ifTrue: [^ self].
	self
		withSiblingsDo: [:each | found
				ifNil: [(each complexContents = anArray first
							or: [anArray first isNil])
						ifTrue: [found := each]]].
	found
		ifNotNil: [
			anArray size = 1
				ifTrue: [^ container listManager addToSelection: found].
			found firstChild
				ifNotNil: [:fc | fc selectNodePath: anArray allButFirst]].

]

{ #category : #accessing }
MorphTreeNodeMorph >> selected [
	^ selected ifNil: [selected := false]
]

{ #category : #accessing }
MorphTreeNodeMorph >> selected: aBoolean [
	selected = aBoolean
		ifTrue: [^ self].
	aBoolean
		ifTrue: [container selectedMorphList add: self]
		ifFalse: [selected 
			ifNotNil: [container selectedMorphList remove: self]].
	selected := aBoolean.

	self complexContents selected: aBoolean
]

{ #category : #private }
MorphTreeNodeMorph >> selectedWithoutNotifyingComplexContents: aBoolean [
	"Only called at creation"
	
	aBoolean ifFalse: [ ^ self ].
	
	container listManager silentlySetSelectedMorph: self.
	selected := aBoolean.
]

{ #category : #accessing }
MorphTreeNodeMorph >> selectionFrame [
	"Answer the selection frame rectangle."

	^ self bounds: self bounds in: container
]

{ #category : #accessing }
MorphTreeNodeMorph >> sensitiveToggleRectangle [
	^(bounds left + self indentGap) @ bounds top extent: (container toggleImageWidth + container gapAfterToggle) @ bounds height
]

{ #category : #accessing }
MorphTreeNodeMorph >> setSelectedSilently: aBoolean [
	
	selected := aBoolean.
	self complexContents selected: aBoolean
]

{ #category : #accessing }
MorphTreeNodeMorph >> spacerWidth [
	"Such a morph composed of, left to right, some space according to the level in the tree, an expand toggle (if any), a check box (if any),  and the item morphs (icon + text often). Compute here the width of the space, including the toggle (and the check box) if there is one."

	| baseRect |
	baseRect := self mustTakeIntoAccountCheckSpace
		ifTrue: [ self checkRectangle ]
		ifFalse: [ self toggleRectangle ].
	^ (self mustTakeIntoAccountToggleSpace or: [ self mustTakeIntoAccountCheckSpace ])
		ifTrue: [ baseRect right + container gapAfterToggle - bounds left ]
		ifFalse: [ baseRect left - bounds left ]
]

{ #category : #private }
MorphTreeNodeMorph >> takeHighlight [

	container listManager lastClickedMorph: self.
	container selectionChanged
]

{ #category : #accessing }
MorphTreeNodeMorph >> theme [
	"Answer the ui theme that provides controls.
	Done directly here to avoid performance hit of
	looking up in window."

	^ Smalltalk ui theme
]

{ #category : #updating }
MorphTreeNodeMorph >> themeChanged [
 
	self allMorphsDo: [ :m | (m isKindOf: StringMorph) ifTrue: [ m color: self theme textColor ] ].
	
	super themeChanged
]

{ #category : #updating }
MorphTreeNodeMorph >> toggleExpandedState [
	| toDelete |
	self isExpanded: self isExpanded not.
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [ firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: toDelete ] ].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ])
		ifFalse: [ ^ self changed ].
	self expand
]

{ #category : #accessing }
MorphTreeNodeMorph >> toggleImageForm [
	^ isExpanded 
			ifTrue: [container expandedFormForMorph: self]
			ifFalse: [container notExpandedFormForMorph: self] 
				
		

]

{ #category : #accessing }
MorphTreeNodeMorph >> toggleRectangle [
	^(bounds left + self indentGap) @ bounds top extent: (container toggleImageWidth) @ bounds height
]

{ #category : #accessing }
MorphTreeNodeMorph >> treeLineDashes [
	^ container treeLineDashes
]

{ #category : #updating }
MorphTreeNodeMorph >> unhighlight [
	complexContents highlightingColor
		ifNotNil: [
			(self valueOfProperty: #originalColor ifAbsent: [Color black])
				ifNotNil: [:c | self color: c]].
	self
		allMorphsDo: [:m | (m isKindOf: StringMorph)
				ifTrue: [m 
						color: (m
								valueOfProperty: #originalColor
								ifAbsent: [ Smalltalk ui theme textColor ])]].

]

{ #category : #updating }
MorphTreeNodeMorph >> update: aSymbol [

	aSymbol = #select
		ifTrue: [ ^ self selected: true ].
		
	aSymbol = #deselect
		ifTrue: [ ^ self selected: false ].
		
	aSymbol = #takeHighlight
		ifTrue: [ ^ self takeHighlight ].

	super update: aSymbol
]

{ #category : #updating }
MorphTreeNodeMorph >> updateChildren [
	self childrenDo: [:child | child parent: self]
	

]

{ #category : #updating }
MorphTreeNodeMorph >> updateChildrenRecursively [
	self childrenDo: [:child | 
		child parent: self. 
		child updateChildrenRecursively]
	

]

{ #category : #updating }
MorphTreeNodeMorph >> updateColumnMorphsWidthWith: aListOfWidths [
	| sw |
	(container columns isEmpty or: [self hasSubmorphs not]) ifTrue: [^ self].
	sw := self spacerWidth.
	1 to: aListOfWidths size - 1 
		do: [:idx | | w | 
			w := aListOfWidths at: idx.
			(controls at: idx) width: (w - (idx = 1 ifTrue: [sw] ifFalse: [0]))].

]

{ #category : #updating }
MorphTreeNodeMorph >> withSiblingsDo: aBlock [

	| node |
	node := self.
	[node isNil] 
		whileFalse: [
			aBlock value: node.
			node := node nextSibling].
]

{ #category : #accessing }
MorphTreeNodeMorph >> withoutListWrapper [

	^complexContents withoutListWrapper
]
