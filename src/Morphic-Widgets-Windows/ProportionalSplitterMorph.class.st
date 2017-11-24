"
I am the morph the user grabs to adjust pane splitters.
"
Class {
	#name : #ProportionalSplitterMorph,
	#superclass : #AbstractResizerMorph,
	#instVars : [
		'leftOrTop',
		'rightOrBottom',
		'splitsTopAndBottom',
		'oldColor',
		'traceMorph'
	],
	#classVars : [
		'ShowHandles'
	],
	#category : #'Morphic-Widgets-Windows-Resizing'
}

{ #category : #preferences }
ProportionalSplitterMorph class >> fastSplitterResize [
	
	^ self theme settings fastDragging
]

{ #category : #preferences }
ProportionalSplitterMorph class >> showSplitterHandles [

	^ ShowHandles ifNil: [ShowHandles := false]
]

{ #category : #preferences }
ProportionalSplitterMorph class >> showSplitterHandles: aBoolean [

	ShowHandles := aBoolean
]

{ #category : #'accessing - defaults' }
ProportionalSplitterMorph class >> splitterWidth [

	^ 4
]

{ #category : #operations }
ProportionalSplitterMorph >> addLeftOrTop: aMorph [

	leftOrTop add: aMorph
]

{ #category : #operations }
ProportionalSplitterMorph >> addRightOrBottom: aMorph [

	rightOrBottom add: aMorph.
	
	
]

{ #category : #operations }
ProportionalSplitterMorph >> beSplitsTopAndBottom [

	splitsTopAndBottom := true.
	
]

{ #category : #accessing }
ProportionalSplitterMorph >> bottomBoundary [
	"Answer the bottom boundary position by calculating the minimum
	of the minimum heights of the bottom morphs."
	
	|morphs|
	morphs := rightOrBottom reject: [:m |
		m layoutFrame bottomFraction ~= 1 and: [
			m layoutFrame topFraction = m layoutFrame bottomFraction]].
	morphs ifEmpty: [
		^(self splitterBelow
			ifNil: [self containingWindow panelRect bottom]
			ifNotNil: [self splitterBelow first top]) - 25].
	^(morphs collect: [:m |
		m bottom - m minExtent y -
			(m layoutFrame topOffset ifNil: [0]) +
			(self layoutFrame bottomOffset ifNil: [0])]) min - self class splitterWidth
]

{ #category : #drawing }
ProportionalSplitterMorph >> drawOn: aCanvas [ 

	| dotBounds size alphaCanvas dotSize |
	self shouldDraw ifFalse: [^self].
	super drawOn: aCanvas.
	self class showSplitterHandles ifTrue: [
	size := self splitsTopAndBottom
				ifTrue: [self handleSize transposed]
				ifFalse: [self handleSize].
	dotSize := self splitsTopAndBottom
				ifTrue: [6 @ self class splitterWidth]
				ifFalse: [self class splitterWidth @ 6].
	alphaCanvas := aCanvas asAlphaBlendingCanvas: 0.7.
	dotBounds := Rectangle center: self bounds center extent: size.
	alphaCanvas fillRectangle: dotBounds color: self handleColor.
	dotBounds := Rectangle center: self bounds center extent: dotSize.
	alphaCanvas fillRectangle: dotBounds color: self dotColor]
]

{ #category : #accessing }
ProportionalSplitterMorph >> getOldColor [
	^ oldColor ifNil: [Color transparent]
]

{ #category : #accessing }
ProportionalSplitterMorph >> handleRect [

	^ Rectangle
		center: self bounds center 
		extent: (self splitsTopAndBottom
			ifTrue: [self handleSize transposed] 
			ifFalse: [self handleSize])
]

{ #category : #accessing }
ProportionalSplitterMorph >> handleSize [

	^ self class splitterWidth @ 30
]

{ #category : #initialization }
ProportionalSplitterMorph >> initialize [

	super initialize.
	
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	splitsTopAndBottom := false.
	
	leftOrTop := OrderedCollection new.
	rightOrBottom := OrderedCollection new
]

{ #category : #testing }
ProportionalSplitterMorph >> isCursorOverHandle [
	^ self class showSplitterHandles not or: [self handleRect containsPoint: ActiveHand cursorPoint]
]

{ #category : #accessing }
ProportionalSplitterMorph >> leftBoundary [
	"Answer the left boundary position by calculating the minimum
	of the minimum widths of the left morphs."
	
	|morphs|
	morphs := leftOrTop reject: [:m |
		m layoutFrame leftFraction ~= 0 and: [
			m layoutFrame leftFraction = m layoutFrame rightFraction]].
	morphs ifEmpty: [
		^(self splitterLeft
			ifNil: [self containingWindow panelRect left]
			ifNotNil: [:s | s left]) + 25].
	^(morphs collect: [:m |
		m left + m minExtent x +
			(self layoutFrame leftOffset ifNil: [0]) -
			(m layoutFrame rightOffset ifNil: [0])]) max
]

{ #category : #'event handling' }
ProportionalSplitterMorph >> mouseDown: anEvent [ 
	"A mouse button has been pressed.
	Update the color for feedback and store the mouse
	position and relative offset to the receiver."
	
	|cp|
	(self class showSplitterHandles not
			and: [self bounds containsPoint: anEvent cursorPoint])
		ifTrue: [oldColor := self color.
			self setGrabbedColor].
	cp := anEvent cursorPoint.
	lastMouse := {cp. cp - self position}
]

{ #category : #'event handling' }
ProportionalSplitterMorph >> mouseMove: anEvent [ 
	anEvent hand temporaryCursor
		ifNil: [^ self].
	self class fastSplitterResize
		ifFalse:  [self updateFromEvent: anEvent]
		ifTrue: [traceMorph
				ifNil: [traceMorph := Morph newBounds: self bounds.
					traceMorph borderColor: Color lightGray.
					traceMorph borderWidth: 1.
					self owner addMorph: traceMorph].
			splitsTopAndBottom
				ifTrue: [traceMorph position: traceMorph position x @ (self normalizedY: anEvent cursorPoint y)]
				ifFalse: [traceMorph position: (self normalizedX: anEvent cursorPoint x) @ traceMorph position y]]
]

{ #category : #operations }
ProportionalSplitterMorph >> normalizedX: x [

	^ (x max: self leftBoundary) min: self rightBoundary
]

{ #category : #operations }
ProportionalSplitterMorph >> normalizedY: y [

	^ (y max: self topBoundary) min: self bottomBoundary
]

{ #category : #actions }
ProportionalSplitterMorph >> resizeCursor [

	^ Cursor resizeForEdge: (splitsTopAndBottom
		ifTrue: [#top]
		ifFalse: [#left])
		
]

{ #category : #accessing }
ProportionalSplitterMorph >> rightBoundary [
	"Answer the right boundary position by calculating the minimum
	of the minimum widths of the right morphs."
	
	|morphs|
	morphs := rightOrBottom reject: [:m |
		m layoutFrame rightFraction ~= 1 and: [
			m layoutFrame leftFraction = m layoutFrame rightFraction]].
	morphs ifEmpty: [
		^(self splitterRight
			ifNil: [self containingWindow panelRect right]
			ifNotNil: [:s | s left]) + 25].
	^(morphs collect: [:m |
		m right - m minExtent x -
			(m layoutFrame leftOffset ifNil: [0]) +
			(self layoutFrame rightOffset ifNil: [0])]) min - self class splitterWidth
]

{ #category : #accessing }
ProportionalSplitterMorph >> siblingSplitters [

	^ self owner 
		submorphsSatisfying: [:each | (each isKindOf: self class) 
									and: [self splitsTopAndBottom = each splitsTopAndBottom and: [each ~= self]]]
]

{ #category : #accessing }
ProportionalSplitterMorph >> splitsTopAndBottom [

	^ splitsTopAndBottom
]

{ #category : #accessing }
ProportionalSplitterMorph >> splitterAbove [
	"Answer the splitter above the receiver that overlaps in its horizontal range."
	
	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each top > self top and: [self overlapsHorizontal: each]]) asSortedCollection: [:a :b | a top < b top]).
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]
]

{ #category : #accessing }
ProportionalSplitterMorph >> splitterBelow [
	"Answer the splitter below the receiver that overlaps in its horizontal range."

	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each top < self top and: [self overlapsHorizontal: each]]) asSortedCollection: [:a :b | a top > b top]).
	
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]
]

{ #category : #accessing }
ProportionalSplitterMorph >> splitterLeft [
	"Answer the splitter to the left of the receiver that overlaps in its vertical range."
	
	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each left < self left and: [self overlapsVertical: each]]) asSortedCollection: [:a :b | a left > b left]).
	
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]
]

{ #category : #accessing }
ProportionalSplitterMorph >> splitterRight [
	"Answer the splitter to the right of the receiver that overlaps in its vertical range."
	
	|splitters|
	splitters := ((self siblingSplitters select: [:each |
		each left > self left and: [self overlapsVertical: each]]) asSortedCollection: [:a :b | a left < b left]).
	
	^ splitters
		ifEmpty: [ nil ]
		ifNotEmpty: [:s | s first ]
]

{ #category : #accessing }
ProportionalSplitterMorph >> topBoundary [
	"Answer the top boundary position by calculating the minimum
	of the minimum heights of the top morphs."
	
	|morphs|
	morphs := leftOrTop reject: [:m |
		m layoutFrame topFraction ~= 0 and: [
			m layoutFrame topFraction = m layoutFrame bottomFraction]].
	morphs ifEmpty: [
		^(self splitterAbove
			ifNil: [self containingWindow panelRect top]
			ifNotNil: [:s | s first top]) + 25].
	^(morphs collect: [:m |
		m top + m minExtent y +
			(self layoutFrame topOffset ifNil: [0]) -
			(m layoutFrame bottomOffset ifNil: [0])]) max
]

{ #category : #'event handling' }
ProportionalSplitterMorph >> updateFromEvent: anEvent [
	"Update the splitter and attached morph positions from the mouse event.
	Take into account the mouse down offset."
	
	| pNew pOld delta selfTop selfBottom selfLeft selfRight|
	pNew := anEvent cursorPoint - lastMouse second.
	pOld := lastMouse first - lastMouse second.
	delta := splitsTopAndBottom
		ifTrue: [0 @ ((self normalizedY: pNew y) - pOld y)]
		ifFalse: [(self normalizedX: pNew x) - pOld x @ 0].
	lastMouse at: 1 put: (splitsTopAndBottom
		ifTrue: [pNew x @ (self normalizedY: pNew y) + lastMouse second]
		ifFalse: [(self normalizedX: pNew x) @ pNew y + lastMouse second]).
	leftOrTop do: [:each | | firstRight firstBottom | 
		firstRight := each layoutFrame rightOffset
					ifNil: [0].
		firstBottom := each layoutFrame bottomOffset
					ifNil: [0].
		each layoutFrame rightOffset: firstRight + delta x.
		each layoutFrame bottomOffset: firstBottom + delta y.
		(each layoutFrame leftFraction = each layoutFrame rightFraction and: [
				each layoutFrame leftFraction ~= 0]) "manual splitter"
			ifTrue: [each layoutFrame leftOffset: (each layoutFrame leftOffset ifNil: [0]) + delta x].
		(each layoutFrame topFraction = each layoutFrame bottomFraction and: [
				each layoutFrame topFraction ~= 0]) "manual splitter"
			ifTrue: [each layoutFrame topOffset: (each layoutFrame topOffset ifNil: [0]) + delta y]].
	rightOrBottom do: [:each | | secondLeft secondTop | 
		secondLeft := each layoutFrame leftOffset
					ifNil: [0].
		secondTop := each layoutFrame topOffset
					ifNil: [0].
		each layoutFrame leftOffset: secondLeft + delta x.
		each layoutFrame topOffset: secondTop + delta y.
		(each layoutFrame leftFraction = each layoutFrame rightFraction and: [
				each layoutFrame rightFraction ~= 1]) "manual splitter"
			ifTrue: [each layoutFrame rightOffset: (each layoutFrame rightOffset ifNil: [0]) + delta x].
		(each layoutFrame topFraction = each layoutFrame bottomFraction and: [
				each layoutFrame bottomFraction ~= 1]) "manual splitter"
			ifTrue: [each layoutFrame bottomOffset: (each layoutFrame bottomOffset ifNil: [0]) + delta y]].
	selfTop := self layoutFrame topOffset ifNil: [0].
	selfBottom := self layoutFrame bottomOffset ifNil: [0].
	selfLeft := self layoutFrame leftOffset ifNil: [0].
	selfRight := self layoutFrame rightOffset ifNil: [0].
	self layoutFrame topOffset: selfTop + delta y.
	self layoutFrame bottomOffset: selfBottom + delta y.
	self layoutFrame leftOffset: selfLeft + delta x.
	self layoutFrame rightOffset: selfRight + delta x.
	self owner layoutChanged
]
