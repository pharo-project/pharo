"
A selectionMorph supports the selection of multiple objects in a morphic world or pasteUp. Using command+shift you get a squared lazzo and you can grab morphs. After you can grab the selection as a morph.

Structure:
	selectedItems	an OrderedCollection of Morphs
					These are the morphs that have been selected
	slippage		a Point
					Keeps track of actual movement between the 
					steps of gridded movement
	dupLoc		a Point
					Notes the position when first duplicate request occurs from halo
	dupDelta	a Point
					Holds the final delta of the first duplicate plus subsequent moves.

"
Class {
	#name : #SelectionMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'selectedItems',
		'slippage',
		'dupLoc',
		'dupDelta',
		'itemsAlreadySelected',
		'otherSelection',
		'undoProperties'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #'dropping/grabbing' }
SelectionMorph >> aboutToBeGrabbedBy: aHand [
	slippage := 0@0.
	^ super aboutToBeGrabbedBy: aHand

]

{ #category : #'halo commands' }
SelectionMorph >> addCustomMenuItems: aMenu hand: aHandMorph [
	"Add custom menu items to the menu"

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'add or remove items' target: self selector: #addOrRemoveItems: argument: aHandMorph.
	aMenu addList: {
		#-.
		{'Place into a row' . #organizeIntoRow}.
		{'Place into a column' . #organizeIntoColumn}.
		#-.
		{'Align left edges' . #alignLeftEdges}.
		{'Align top edges' . #alignTopEdges}.
		{'Align right edges' . #alignRightEdges}.
		{'Align bottom edges' . #alignBottomEdges}.
		#-.
		{'Align centers vertically' . #alignCentersVertically}.
		{'Align centers horizontally' . #alignCentersHorizontally}.
		}.

	self selectedItems size > 2
		ifTrue: [
			aMenu addList: {
				#-.
				{'Distribute vertically' . #distributeVertically}.
				{'Distribute horizontally' . #distributeHorizontally}.
				}.
		].

]

{ #category : #'halos and balloon help' }
SelectionMorph >> addHandlesTo: aHaloMorph box: box [
	| onlyThese |
	aHaloMorph haloBox: box.
	onlyThese := #(addDismissHandle: addMenuHandle: addGrabHandle: addDragHandle: addDupHandle: addHelpHandle: addGrowHandle: addFontSizeHandle: addFontStyleHandle: addFontEmphHandle: addRecolorHandle:).
	HaloMorph currentHaloSpecifications do:
		[:aSpec | (onlyThese includes: aSpec addHandleSelector) ifTrue:
				[aHaloMorph perform: aSpec addHandleSelector with: aSpec]].
	aHaloMorph innerTarget addOptionalHandlesTo: aHaloMorph box: box
]

{ #category : #'halos and balloon help' }
SelectionMorph >> addOptionalHandlesTo: aHalo box: box [
	aHalo addHandleAt: box leftCenter color: Color blue icon: nil
		on: #mouseUp send: #addOrRemoveItems: to: self.
]

{ #category : #'halo commands' }
SelectionMorph >> addOrRemoveItems: handOrEvent [ 
	"Make a new selection extending the current one."

	| hand |
	hand := (handOrEvent isMorphicEvent) 
				ifFalse: [handOrEvent]
				ifTrue: [handOrEvent hand].
	hand 
		addMorphBack: ((self class 
				newBounds: (hand lastEvent cursorPoint extent: 16 @ 16)) 
					setOtherSelection: self).

]

{ #category : #'halo commands' }
SelectionMorph >> alignBottomEdges [
	"Make the bottom coordinate of all my elements be the same"

	| maxBottom |
	maxBottom := (selectedItems collect: [:itm | itm bottom]) max.
	selectedItems do:
		[:itm | itm bottom: maxBottom].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> alignCentersHorizontally [
	"Make every morph in the selection have the same vertical center as the topmost item."

	| minLeft leftMost |
	selectedItems size > 1 ifFalse: [^ self].
	minLeft := (selectedItems collect: [:itm | itm left]) min.
	leftMost := selectedItems detect: [:m | m left = minLeft].
	selectedItems do:
		[:itm | itm center: (itm center x @ leftMost center y)].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> alignCentersVertically [
	"Make every morph in the selection have the same horizontal center as the topmost item."

	| minTop topMost |
	selectedItems size > 1 ifFalse: [^ self].
	minTop := (selectedItems collect: [:itm | itm top]) min.
	topMost := selectedItems detect: [:m | m top = minTop].
	selectedItems do:
		[:itm | itm center: (topMost center x @ itm center y)].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> alignLeftEdges [
	"Make the left coordinate of all my elements be the same"

	| minLeft |
	minLeft := (selectedItems collect: [:itm | itm left]) min.
	selectedItems do:
		[:itm | itm left: minLeft].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> alignRightEdges [
	"Make the right coordinate of all my elements be the same"

	| maxRight |
	maxRight := (selectedItems collect: [:itm | itm right]) max.
	selectedItems do:
		[:itm | itm right: maxRight].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> alignTopEdges [
	"Make the top coordinate of all my elements be the same"

	| minTop |
	minTop := (selectedItems collect: [:itm | itm top]) min.
	selectedItems do:
		[:itm | itm top: minTop].

	self changed

]

{ #category : #'halos and balloon help' }
SelectionMorph >> balloonHelpTextForHandle: aHandle [
	(aHandle eventHandler mouseSelectorsInclude: #addOrRemoveItems:)
		ifTrue: [^'Add items to, or remove them from, this selection.'].
	^ super balloonHelpTextForHandle: aHandle
]

{ #category : #accessing }
SelectionMorph >> borderColor: aColor [

	| bordered |
	bordered := selectedItems.
	undoProperties ifNil: [undoProperties := bordered collect: [:m | m borderColor]].
	bordered do: [:m | m borderColor: aColor]
]

{ #category : #undo }
SelectionMorph >> borderColorForItems: colorCollection [

	(selectedItems select: [:m | m isKindOf: BorderedMorph])
		with: colorCollection
		do: [:m :c | m borderColor: c]
]

{ #category : #accessing }
SelectionMorph >> borderWidth: aWidth [

	| bordered |
	bordered := selectedItems select: [:m | m isKindOf: BorderedMorph].
	undoProperties ifNil: [undoProperties := bordered collect: [:m | m borderWidth]].
	bordered do: [:m | m borderWidth: aWidth]
]

{ #category : #undo }
SelectionMorph >> borderWidthForItems: widthCollection [

	(selectedItems select: [:m | m isKindOf: BorderedMorph])
		with: widthCollection
		do: [:m :c | m borderWidth: c]
]

{ #category : #geometry }
SelectionMorph >> bounds: newBounds [
	"Make sure position: gets called before extent:; Andreas' optimization for growing/shrinking in ChangeSet 3119 screwed up selection of morphs from underlying pasteup."

	selectedItems := OrderedCollection new.  "Avoid repostioning items during super position:"
	self position: newBounds topLeft; extent: newBounds extent

]

{ #category : #initialization }
SelectionMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ (self defaultColor) twiceDarker alpha: 0.75
]

{ #category : #initialization }
SelectionMorph >> defaultColor [
	"answer the default color/fill style for the receiver"

	^ self theme menuSelectionColor alpha: 0.08
]

{ #category : #'submorphs-add/remove' }
SelectionMorph >> delete [
	self setProperty: #deleting toValue: true.
	super delete.
	
]

{ #category : #'submorphs-add/remove' }
SelectionMorph >> dismissViaHalo [

	super dismissViaHalo.
	selectedItems do: [:m | m dismissViaHalo]
]

{ #category : #'halo commands' }
SelectionMorph >> distributeHorizontally [
	"Distribute the empty vertical space in a democratic way."
	| minLeft maxRight totalWidth currentLeft space |

	self selectedItems size > 2
		ifFalse: [^ self].

	minLeft := self selectedItems anyOne left.
	maxRight := self selectedItems anyOne right.
	totalWidth := 0.
	self selectedItems
		do: [:each | 
			minLeft := minLeft min: each left.
			maxRight := maxRight max: each right.
			totalWidth := totalWidth + each width].

	currentLeft := minLeft.
	space := (maxRight - minLeft - totalWidth / (self selectedItems size - 1)) rounded.
	(self selectedItems
		asSortedCollection: [:x :y | x left <= y left])
		do: [:each | 
			each left: currentLeft.
			currentLeft := currentLeft + each width + space].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> distributeVertically [
	"Distribute the empty vertical space in a democratic way."
	| minTop maxBottom totalHeight currentTop space |
	self selectedItems size > 2
		ifFalse: [^ self].

	minTop := self selectedItems anyOne top.
	maxBottom := self selectedItems anyOne bottom.
	totalHeight := 0.
	self selectedItems
		do: [:each | 
			minTop := minTop min: each top.
			maxBottom := maxBottom max: each bottom.
			totalHeight := totalHeight + each height].

	currentTop := minTop.
	space := (maxBottom - minTop - totalHeight / (self selectedItems size - 1)) rounded.
	(self selectedItems asSortedCollection:[:x :y | x top <= y top])
		do: [:each | 
			each top: currentTop.
			currentTop := currentTop + each height + space].

	self changed

]

{ #category : #'halo commands' }
SelectionMorph >> doDup: evt fromHalo: halo handle: dupHandle [

	selectedItems := selectedItems collect: #duplicate.
	selectedItems do: [ :m | self owner addMorph: m ].
	dupDelta
		ifNil: [ "First duplicate operation -- note starting location"
			dupLoc := self position.
			evt hand grabMorph: self.
			halo removeAllHandlesBut: dupHandle
			]
		ifNotNil: [ "Subsequent duplicate does not grab, but only moves me and my morphs"
			dupLoc := nil.
			self position: self position + dupDelta
			]
]

{ #category : #private }
SelectionMorph >> doneExtending [

	otherSelection ifNotNil:
		[selectedItems := otherSelection selectedItems , selectedItems.
		otherSelection delete.
		self setOtherSelection: nil].
	self changed.
	self layoutChanged.
	super privateBounds:
		((Rectangle merging: (selectedItems collect: [:m | m fullBounds]))
			expandBy: 8).
	self changed.
	self addHalo.
]

{ #category : #drawing }
SelectionMorph >> drawOn: aCanvas [

	| canvas form1 form2 box |
	super drawOn: aCanvas.
	box := self bounds.
	selectedItems do: [:m | box := box merge: m fullBounds].
	box := box expandBy: 1.
	canvas := Display defaultCanvasClass extent: box extent depth: 8.
	canvas translateBy: box topLeft negated
		during: [:tempCanvas | selectedItems do: [:m | tempCanvas fullDrawMorph: m]].
	form1 := (Form extent: box extent) copyBits: (0@0 extent: box extent) from: canvas form at: 0@0 colorMap: (Color maskingMap: 8).
	form2 := Form extent: box extent.
	(0@0) fourNeighbors do: [:d | form1 displayOn: form2 at: d rule: Form under].
	form1 displayOn: form2 at: 0@0 rule: Form erase.
	aCanvas stencil: form2
		at: box topLeft
		sourceRect: form2 boundingBox
		color: self borderColor

]

{ #category : #'halo commands' }
SelectionMorph >> duplicate [
	"Make a duplicate of the receiver and havbe the hand grab it"

	selectedItems :=  selectedItems collect: #duplicate.
	selectedItems reverseDo: [:m | (owner ifNil: [self currentWorld]) addMorph: m].
	dupLoc := self position.
	ActiveHand grabMorph: self.
	
]

{ #category : #initialization }
SelectionMorph >> extendByHand: aHand [
	"Assumes selection has just been created and added to some pasteUp or world"
	| startPoint handle |

	startPoint := self position.

	handle := NewHandleMorph new followHand: aHand
		forEachPointDo: [:newPoint |
					| localPt |
					localPt := (self transformFrom: self world) globalPointToLocal: newPoint.
					self bounds: (startPoint rectangle: localPt)
				]
		lastPointDo: [:newPoint |
					selectedItems isEmpty
						ifTrue: [self delete]
						ifFalse: [
							selectedItems size = 1
								ifTrue:[self delete.  selectedItems anyOne addHalo]
								ifFalse:[self doneExtending]
						]
				].

	aHand attachMorph: handle.
	handle startStepping.
]

{ #category : #geometry }
SelectionMorph >> extent: newExtent [

	super extent: newExtent.
	self selectSubmorphsOf: self pasteUpMorph 
]

{ #category : #viewer }
SelectionMorph >> externalName [
	^ 'Selected {1} objects' translated format:{self selectedItems size}
]

{ #category : #'visual properties' }
SelectionMorph >> fillStyle: aColor [
	undoProperties ifNil: [undoProperties := selectedItems collect: [:m | m fillStyle]].
	selectedItems do: [:m | m fillStyle: aColor]
]

{ #category : #undo }
SelectionMorph >> fillStyleForItems: fillStyleCollection [

	selectedItems with: fillStyleCollection do: [:m :c | m fillStyle: c]
]

{ #category : #'submorphs-add/remove' }
SelectionMorph >> goBehind [
	"selection morph stays on top"

	
]

{ #category : #'halos and balloon help' }
SelectionMorph >> hasHalo: aBool [ 
	super hasHalo: aBool.
	aBool
		ifFalse: [ (self hasProperty: #deleting) ifFalse: [self delete] ]

]

{ #category : #initialization }
SelectionMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	
	selectedItems := OrderedCollection new.
	itemsAlreadySelected := OrderedCollection new.
	slippage := 0@0
]

{ #category : #'dropping/grabbing' }
SelectionMorph >> justDroppedInto: newOwner event: evt [

	selectedItems isEmpty ifTrue:
		["Hand just clicked down to draw out a new selection"
		^ self extendByHand: evt hand].
	dupLoc ifNotNil: [dupDelta := self position - dupLoc].
	selectedItems reverseDo: [:m | 
		self defer:
			[m referencePosition: (newOwner localPointToGlobal: m referencePosition).
			newOwner handleDropMorph:
				(DropEvent new setPosition: evt cursorPoint contents: m hand: evt hand)]].
	evt wasHandled: true
]

{ #category : #menus }
SelectionMorph >> maybeAddCollapseItemTo: aMenu [
	"... don't "
]

{ #category : #'wiw support' }
SelectionMorph >> morphicLayerNumber [
	"helpful for insuring some morphs always appear in front of or  
	behind others. smaller numbers are in front"
	^ 8
]

{ #category : #'halo commands' }
SelectionMorph >> organizeIntoColumn [
	"Place my objects in a column-enforcing container"

	((AlignmentMorph inAColumn: (selectedItems asSortedCollection: [:x :y | x top < y top])) setNameTo: 'Column'; color: Color orange muchLighter; enableDragNDrop: true; yourself) openInHand

]

{ #category : #'halo commands' }
SelectionMorph >> organizeIntoRow [
	"Place my objects in a row-enforcing container"

	((AlignmentMorph inARow: (selectedItems asSortedCollection: [:x :y | x left < y left])) setNameTo: 'Row'; color: Color orange muchLighter; enableDragNDrop: true; yourself) openInHand

]

{ #category : #private }
SelectionMorph >> privateFullMoveBy: delta [
	| deltaSlipped griddingMorph |
	selectedItems isEmpty
		ifTrue: [ ^ super privateFullMoveBy: delta ].
	griddingMorph := self pasteUpMorph.
	griddingMorph ifNil: [ ^ super privateFullMoveBy: delta ].
	deltaSlipped := delta + slippage.
	slippage := 0.
	super privateFullMoveBy: deltaSlipped.
	selectedItems do: [ :m | m position: m position + deltaSlipped ]
]

{ #category : #private }
SelectionMorph >> selectSubmorphsOf: aMorph [

	| newItems removals |
	newItems := aMorph submorphs select:
		[:m | (bounds containsRect: m fullBounds) 
					and: [m~~self
					and: [(m isKindOf: HaloMorph) not]]].
	otherSelection ifNil: [^ selectedItems := newItems].

	removals := newItems intersection: itemsAlreadySelected.
	otherSelection setSelectedItems: (itemsAlreadySelected copyWithoutAll: removals).
	selectedItems := (newItems copyWithoutAll: removals).

]

{ #category : #private }
SelectionMorph >> selectedItems [

	^ selectedItems
]

{ #category : #private }
SelectionMorph >> setOtherSelection: otherOrNil [

	otherSelection := otherOrNil.
	otherOrNil
		ifNil: [ super borderColor: Color blue ]
		ifNotNil: [ itemsAlreadySelected := otherSelection selectedItems.
			super borderColor: Color green
			]
]

{ #category : #private }
SelectionMorph >> setSelectedItems: items [

	selectedItems := items.
	self changed
]

{ #category : #accessing }
SelectionMorph >> wantsToBeTopmost [
	"Answer if the receiver want to be one of the topmost objects in 
	its owner"
	^ true
]
