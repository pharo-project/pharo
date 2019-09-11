"
An IndentingListItemMorph is a StringMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph.

It will also display lines around the toggle depending on UITheme settings

Instance variables:

indentLevel <SmallInteger> 	the indent level, from 0 at the root and increasing by 1 at each level of the hierarchy.

isExpanded <Boolean>		true if this item is expanded (showing its children)

complexContents <ListItemWrapper>	an adapter wrapping my represented item that can answer its children, etc.
	
firstChild <IndentingListItemMorph|nil>	my first child, or nil if none
	
container <SimpleHierarchicalListMorph>	my container
	
nextSibling <IndentingListItemMorph|nil>	the next item in the linked list of siblings, or nil if none.

Contributed by Bob Arning as part of the ObjectExplorer package.
Don't blame him if it's not perfect.  We wanted to get it out for people to play with.
"
Class {
	#name : #IndentingListItemMorph,
	#superclass : #StringMorph,
	#instVars : [
		'indentLevel',
		'isExpanded',
		'complexContents',
		'firstChild',
		'container',
		'nextSibling',
		'icon'
	],
	#category : #'Morphic-Widgets-List'
}

{ #category : #'drag and drop' }
IndentingListItemMorph >> acceptDroppingMorph: toDrop event: evt [
	complexContents acceptDroppingObject: toDrop complexContents.
	toDrop delete.
	self highlightForDrop: false.
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode delete].
	].
	firstChild := nil.
	complexContents hasContents ifFalse: [^self].
	firstChild := hostList 
		addMorphsTo: morphList
		from: complexContents contents 
		allowSorting: true
		withExpandedItems: expandedItems
		atLevel: indentLevel + 1.
	
]

{ #category : #accessing }
IndentingListItemMorph >> balloonText [

	^complexContents balloonText ifNil: [super balloonText]
]

{ #category : #'halos and balloon help' }
IndentingListItemMorph >> boundsForBalloon [

	"some morphs have bounds that are way too big"
	container ifNil: [^super boundsForBalloon].
	^self boundsInWorld intersect: container boundsInWorld ifNone: [self boundsInWorld ]
]

{ #category : #accessing }
IndentingListItemMorph >> canExpand [

	^complexContents hasContents
]

{ #category : #accessing }
IndentingListItemMorph >> children [
	| children |
	children := OrderedCollection new.
	self childrenDo: [:each | children add: each].
	^children
]

{ #category : #enumeration }
IndentingListItemMorph >> childrenDo: aBlock [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aBlock value: aNode].
	]
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> complexContents [

	^complexContents
]

{ #category : #drawing }
IndentingListItemMorph >> drawLineToggleToTextOn: aCanvas lineColor: lineColor hasToggle: hasToggle [
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge.
	Only draw now if no toggle."

	| myBounds myCenter hLineY hLineLeft myTheme|
	self isSoleItem ifTrue: [ ^self ].
	self hasToggle ifTrue: [^self].
	myBounds := self toggleBounds.
	myCenter := myBounds center.
	hLineY := myCenter y - 1.
	hLineLeft := myCenter x.
	"Draw line from toggle to text. Use optimised form since vertical."
	myTheme := self theme.
	aCanvas
		frameRectangle: (hLineLeft @ hLineY corner: myBounds right + 3 @ (hLineY + 1))
		width: myTheme treeLineWidth
		colors: (myTheme treeLineColorsFrom: lineColor)
		dashes: myTheme treeLineDashes
]

{ #category : #drawing }
IndentingListItemMorph >> drawLinesOn: aCanvas lineColor: lineColor [ 
	| hasToggle |
	hasToggle := self hasToggle.
	"Draw line from toggle to text"
	self drawLineToggleToTextOn: aCanvas lineColor: lineColor hasToggle: hasToggle.

	"Draw the line from my toggle to the nextSibling's toggle"
	self nextSibling ifNotNil: [ self drawLinesToNextSiblingOn: aCanvas lineColor: lineColor hasToggle: hasToggle ].

	"If I have children and am expanded, draw a line to my first child"
	(self firstChild notNil and: [ self isExpanded ])
		ifTrue: [ self drawLinesToFirstChildOn: aCanvas lineColor: lineColor]
]

{ #category : #drawing }
IndentingListItemMorph >> drawLinesToFirstChildOn: aCanvas lineColor: lineColor [
	"Draw line from me to first child.
	Don't bother if the first child has a toggle.."

	| vLineX vLineTop vLineBottom childBounds childCenter myTheme |
	self firstChild hasToggle
		ifTrue: [ ^ self ].
	childBounds := self firstChild toggleBounds.
	childCenter := childBounds center.
	vLineX := childCenter x.
	vLineTop := bounds bottom.
	vLineBottom := self firstChild hasToggle
		ifTrue: [ childCenter y - (childBounds height // 2) + 1 ]
		ifFalse: [ childCenter y - 2 ].
	myTheme := self theme.
	aCanvas
		frameRectangle: (vLineX @ vLineTop corner: (vLineX + 1) @ vLineBottom)
		width: myTheme treeLineWidth
		colors: (myTheme treeLineColorsFrom: lineColor)
		dashes: myTheme treeLineDashes
]

{ #category : #drawing }
IndentingListItemMorph >> drawLinesToNextSiblingOn: aCanvas lineColor: lineColor hasToggle: hasToggle [
	"Draw line from me to next sibling"
	
	| myBounds nextSibBounds vLineX myCenter vLineTop vLineBottom myTheme|
	myBounds := self toggleBounds.
	nextSibBounds := self nextSibling toggleBounds.
	myCenter := myBounds center.
	vLineX := myCenter x.
	vLineTop := myCenter y + 1.
	vLineBottom := nextSibBounds center y - 1.
	"Draw line from me to next sibling"
	myTheme := self theme.
	aCanvas
		frameRectangle: (vLineX @ vLineTop corner: vLineX + 1 @ vLineBottom)
		width: myTheme treeLineWidth
		colors: (myTheme treeLineColorsFrom: lineColor)
		dashes: myTheme treeLineDashes
]

{ #category : #drawing }
IndentingListItemMorph >> drawOn: aCanvas [

	| tRect sRect columnScanner colorToUse columnLeft |

	tRect := self toggleRectangle.
	sRect := bounds withLeft: tRect right + 4.

	self drawToggleOn: aCanvas in: tRect.
	colorToUse := complexContents preferredColor ifNil: [ self theme textColor ].
	icon
		ifNotNil: [ aCanvas
				translucentImage: icon
				at: sRect left @ ( self top + ( ( self height - icon height ) // 2 ) ).
			sRect := sRect left: sRect left + icon width + 2
			].
	( container columns isNil or: [ ( contents asString indexOf: Character tab ) = 0 ] )
		ifTrue: [ sRect := sRect top: ( sRect top + sRect bottom - self fontToUse height ) // 2.
			contents
				treeRenderOn: aCanvas
				bounds: sRect
				color: colorToUse
				font: self fontToUse
				from: self
			]
		ifFalse: [ columnLeft := sRect left.
			columnScanner := contents asString readStream.
			container columns
				do: [ :width | 
					| columnData columnRect |

					columnRect := columnLeft @ sRect top extent: width @ sRect height.
					columnData := columnScanner upTo: Character tab.
					columnData isEmpty
						ifFalse: [ aCanvas
								drawString: columnData
								in: columnRect
								font: self fontToUse
								color: colorToUse
							].
					columnLeft := columnRect right + 5
					]
			]
]

{ #category : #drawing }
IndentingListItemMorph >> drawToggleOn: aCanvas in: aRectangle [

	| aForm centeringOffset |
	complexContents hasContents ifFalse: [^self].
	aForm := isExpanded 
		ifTrue: [container expandedFormForMorph: self]
		ifFalse: [container notExpandedFormForMorph: self].
	centeringOffset := ((aRectangle height - aForm extent y) / 2.0) truncated.
	^aCanvas 
		translucentImage: aForm 
		at: (aRectangle topLeft translateBy: 0 @ centeringOffset).

]

{ #category : #private }
IndentingListItemMorph >> findExactPathMatchIn: anArray [ 
	self
		withSiblingsDo: [:each | (each complexContents asString = anArray first
					or: [anArray first isNil])
				ifTrue: [^ each]].
	^ nil
]

{ #category : #private }
IndentingListItemMorph >> findPathIn: anArray [ 
	| found |
	found := self findExactPathMatchIn: anArray.
	found
		ifNil: ["try again with no case sensitivity"
			found := self findSimilarPathMatchIn: anArray].
	^ found
]

{ #category : #private }
IndentingListItemMorph >> findSimilarPathMatchIn: anArray [ 
	self
		withSiblingsDo: [:each | (each complexContents asString sameAs: anArray first)
				ifTrue: [^ each]].
	^ nil
]

{ #category : #accessing }
IndentingListItemMorph >> firstChild [

	^firstChild
]

{ #category : #accessing }
IndentingListItemMorph >> hasIcon [
	"Answer whether the receiver has an icon."
	^ icon notNil
]

{ #category : #private }
IndentingListItemMorph >> hasToggle [
	^ complexContents hasContents
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> highlight [

	complexContents highlightingColor ifNotNil: [self color: complexContents highlightingColor].
	self changed.
	

]

{ #category : #accessing }
IndentingListItemMorph >> icon [
	"answer the receiver's icon"
	^ icon
]

{ #category : #'mouse events' }
IndentingListItemMorph >> inToggleArea: aPoint [

	^self toggleRectangle containsPoint: aPoint
]

{ #category : #accessing }
IndentingListItemMorph >> indentLevel [

	^indentLevel
]

{ #category : #initialization }
IndentingListItemMorph >> initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel [

	container := hostList.
	complexContents := anObject.
	indentLevel := newLevel.
	self initWithContents: anObject asString font: StandardFonts listFont emphasis: nil.
	isExpanded := false.
 	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	icon := anObject icon.
	self extent: self minWidth @ self minHeight
]

{ #category : #initialization }
IndentingListItemMorph >> initialize [
"initialize the state of the receiver"
	super initialize.
""
	indentLevel := 0.
	isExpanded := false
]

{ #category : #accessing }
IndentingListItemMorph >> isExpanded [

	^isExpanded
]

{ #category : #accessing }
IndentingListItemMorph >> isExpanded: aBoolean [

	isExpanded := aBoolean
]

{ #category : #accessing }
IndentingListItemMorph >> isFirstItem [
	"I have no idea why the owner of the list can get nil but it happens when the packages are published with the Monticello Browser."
	
	^ owner notNil and: [owner submorphs first == self]
]

{ #category : #accessing }
IndentingListItemMorph >> isSoleItem [
	^self isFirstItem and: [ owner submorphs size = 1 ]
]

{ #category : #accessing }
IndentingListItemMorph >> minHeight [
	"Answer the minimum height of the receiver."
	
	| iconHeight |
	iconHeight := self hasIcon
				ifTrue: [self icon height + 2]
				ifFalse: [0].
	^(( self contents heightToDisplayInTree: self) max: iconHeight) max: super minHeight
]

{ #category : #accessing }
IndentingListItemMorph >> minWidth [
	"Fixed to work such that guessed width is unnecessary in
	#adjustSubmorphPositions."

	| iconWidth |
	iconWidth := self hasIcon
				ifTrue: [self icon width + 2]
				ifFalse: [0].
	^(13 * indentLevel + 15 + (contents widthToDisplayInTree: self)
		+ iconWidth) max: super minWidth
]

{ #category : #accessing }
IndentingListItemMorph >> nextSibling [

	^nextSibling
]

{ #category : #accessing }
IndentingListItemMorph >> nextSibling: anotherMorph [

	nextSibling := anotherMorph
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> openPath: anArray [
	| found |
	anArray isEmpty
		ifTrue: [ ^ container setSelectedMorph: nil ].
	found := self findPathIn: anArray.
	found ifNil: [ ^ container setSelectedMorph: nil ].
	found isExpanded
		ifTrue: [ found refreshExpandedState ]
		ifFalse: [ found toggleExpandedState ].
	container adjustSubmorphPositions.
	found changed.
	anArray size = 1
		ifTrue: [ ^ container setSelectedMorph: found ].
	^ found firstChild
		ifNil: [ container setSelectedMorph: nil ]
		ifNotNil: [ found firstChild openPath: anArray allButFirst ]
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> recursiveAddTo: aCollection [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection].
	].
	aCollection add: self
	
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> recursiveDelete [

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveDelete].
	].
	self delete
	
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> refreshExpandedState [

 	| newChildren toDelete c |
	toDelete := OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: toDelete].
	].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [complexContents hasContents]) ifFalse: [
		^self changed
	].
	(c := complexContents contents) isEmpty ifTrue: [^self changed].
	newChildren := container 
		addSubmorphsAfter: self 
		fromCollection: c 
		allowSorting: true.
	firstChild := newChildren first.

]

{ #category : #search }
IndentingListItemMorph >> searchingString [
	" string used to detect research "

	^ self contents asString
]

{ #category : #private }
IndentingListItemMorph >> toggleBounds [
	^self toggleRectangle
]

{ #category : #'private-container protocol' }
IndentingListItemMorph >> toggleExpandedState [

	isExpanded := isExpanded not.
	self refreshExpandedState.
]

{ #category : #action }
IndentingListItemMorph >> toggleRectangle [

	| h |
	h := bounds height.
	^(bounds left + (13 * indentLevel)) @ bounds top extent: 9@h
]

{ #category : #drawing }
IndentingListItemMorph >> unhighlight [
	complexContents highlightingColor ifNotNil: [ self color: self theme textColor ].
	self changed.
	
	

]

{ #category : #accessing }
IndentingListItemMorph >> userString [
	"Add leading tabs to my userString"
	^ (String new: indentLevel withAll: Character tab), super userString

]

{ #category : #private }
IndentingListItemMorph >> withSiblingsDo: aBlock [

	| node |
	node := self.
	[node isNil] whileFalse: [
		aBlock value: node.
		node := node nextSibling
	].
]

{ #category : #converting }
IndentingListItemMorph >> withoutListWrapper [

	^complexContents withoutListWrapper
]
