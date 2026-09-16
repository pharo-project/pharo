"
I display visual indicators in the editor gutter showing lines that differ from the baseline
(typically the last committed version). I sit at level 3 (leftmost) in the gutter.

Visual feedback per state:
  - #added:    green bar spanning the full line height
  - #modified: blue bar spanning the full line height
  - #removed:  small red right-pointing arrow (▶) between surrounding lines

Line state classification uses TextDiffBuilder>>patchSequenceDoIfMatch:ifInsert:ifRemove:
to walk the LCS patch sequence. Consecutive removes+inserts between matches are grouped
into runs and flushed at each match or end-of-patch:
  - source-only run (removes)  → gap in removedPositions
  - destination-only run (inserts) → #added
  - both source and destination → #modified

Instance Variables:
  baseline         <String> Cached original text to diff against
  baselineProvider <RubMethodBaselineProvider> Pluggable provider for the baseline
  lineStates       <Array of Symbol> Per-line state: #unchanged, #added, #modified
  removedPositions <OrderedCollection> 1-indexed gap positions in current text where a removed-line arrow should appear
  previewMorph     <RubGutterDiffPreviewMorph> Currently visible diff popup (or nil)
  editingMethod    <CompiledMethod> The method being edited (for baseline fetching)
"
Class {
	#name : 'RubGutterDiffDisplayer',
	#superclass : 'RubScrolledTextSideRuler',
	#instVars : [
		'baseline',
		'baselineProvider',
		'lineStates',
		'removedPositions',
		'previewMorph',
		'editingMethod',
		'hoveredState'
	],
	#category : 'Rubric-Editing-Widgets',
	#package : 'Rubric',
	#tag : 'Editing-Widgets'
}

{ #category : 'querying' }
RubGutterDiffDisplayer class >> key [
	^ #gutterDiff
]

{ #category : 'private' }
RubGutterDiffDisplayer >> adjustEventPosition: anEvent [

	| lineIndex line |

	lineIndex := self lineIndexForPoint: anEvent position.
	lineIndex > self lines size ifTrue: [ ^ self ].
	line := self lines at: lineIndex.
	anEvent setPosition: line topLeft
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> baseline [

	^ baseline
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> baseline: aString [

	baseline := aString
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> baselineProvider [

	^ baselineProvider
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> baselineProvider: aProvider [

	baselineProvider := aProvider
]

{ #category : 'private' }
RubGutterDiffDisplayer >> colorForState: aSymbol [

	"TODO: should we move this to the theme?"
	aSymbol = #added ifTrue: [ ^ (Color fromHexString: '2da044') alpha: 0.5 ].
	aSymbol = #modified ifTrue: [ ^ (Color fromHexString: '0078d5') alpha: 0.5 ].
	aSymbol = #removed ifTrue: [ ^ Color fromHexString: 'f95249' ].
	
	^ Color transparent
]

{ #category : 'private' }
RubGutterDiffDisplayer >> computeHoveredStateAt: aPoint [

	| lineIndex state textY |

	lineStates ifNil: [ ^ nil ].
	
	lineIndex := self lineIndexForPoint: aPoint.
	lineIndex > lineStates size ifTrue: [ ^ nil ].
	textY := aPoint y + self offset y - self bounds top.
	
	(self consolidatedRemovedPositions detect: [ :pos |
			 (self gapYForPosition: pos lines: self lines) ifNotNil: [ :gapY | (textY - gapY) abs <= self indicatorWidth ] ifNil: [ false ] ]
		 ifNone: [ nil ]) ifNotNil: [ :pos |
			^ { #removed . pos }
	].

	state := lineStates at: lineIndex.
	state = #unchanged ifTrue: [ ^ nil ].

	^ { state . lineIndex }
]

{ #category : 'private' }
RubGutterDiffDisplayer >> computeLineStates [

	| currentText builder srcLine dstLine srcRunStart srcRunEnd dstRunStart dstRunEnd |
	baseline ifNil: [ ^ self ].
	self textArea ifNil: [ ^ self ].
	currentText := self textArea text asString.
	lineStates := Array new: currentText lines size withAll: #unchanged.
	removedPositions := OrderedCollection new.

	builder := TextDiffBuilder from: baseline to: currentText.
	srcLine := dstLine := 0.
	srcRunStart := srcRunEnd := dstRunStart := dstRunEnd := 0.

	builder
		patchSequenceDoIfMatch: [ :aString |
				(dstRunStart > 0 or: [ srcRunStart > 0 ]) ifTrue: [
						self
							flushDiffRunSrcStart: srcRunStart
							srcEnd: srcRunEnd
							dstStart: dstRunStart
							dstEnd: dstRunEnd
							srcLine: srcLine
							dstLine: dstLine.
						srcRunStart := srcRunEnd := dstRunStart := dstRunEnd := 0 ].
				srcLine := srcLine + 1.
				dstLine := dstLine + 1 ]
		ifInsert: [ :aString |
				srcRunStart > 0 ifTrue: [
						self
							flushDiffRunSrcStart: srcRunStart
							srcEnd: srcRunEnd
							dstStart: dstRunStart
							dstEnd: dstRunEnd
							srcLine: srcLine
							dstLine: dstLine.
						srcRunStart := srcRunEnd := dstRunStart := dstRunEnd := 0 ].
				dstLine := dstLine + 1.
				dstRunStart = 0 ifTrue: [ dstRunStart := dstLine ].
				dstRunEnd := dstLine ]
		ifRemove: [ :aString |
				srcLine := srcLine + 1.
				srcRunStart = 0 ifTrue: [ srcRunStart := srcLine ].
				srcRunEnd := srcLine ].

	(dstRunStart > 0 or: [ srcRunStart > 0 ]) ifTrue: [
			self
				flushDiffRunSrcStart: srcRunStart
				srcEnd: srcRunEnd
				dstStart: dstRunStart
				dstEnd: dstRunEnd
				srcLine: srcLine
				dstLine: dstLine ]
]

{ #category : 'geometry' }
RubGutterDiffDisplayer >> computedWidthFrom: aRectangle [

	^ (self indicatorWidth +
		self horizontalGapBefore +
		self horizontalGapAfter +
		self verticalSeparatorWidth) truncated
]

{ #category : 'private' }
RubGutterDiffDisplayer >> consolidatedRemovedPositions [

	removedPositions ifEmpty: [ ^ #(  ) ].
	^ removedPositions withIndexSelect: [ :pos :idx | idx = 1 or: [
			pos ~= ((removedPositions at: idx - 1) + 1)
		]
	]
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawHoveredIndicatorBarOn: aCanvas forState: aSymbol atLine: aTextLine [
	| indicatorColor indicatorBounds hoverWidth |
	hoveredState ifNil: [ ^ self ].
	hoverWidth := self indicatorWidth + 4.
	indicatorColor := (self colorForState: aSymbol) alpha: 0.8.
	indicatorBounds := (self bounds left + self horizontalGapBefore) @ (aTextLine top + 2)
		corner: (self bounds left + self horizontalGapBefore + hoverWidth) @ (aTextLine bottom - 2).
	indicatorBounds := indicatorBounds
		translateBy: 0 @ (self offset y negated + self bounds top).
	aCanvas fillRectangle: indicatorBounds color: indicatorColor
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawHoveredIndicatorBarOnAthens: aCanvas forState: aSymbol atLine: aTextLine [

	| indicatorColor indicatorBounds hoverWidth |

	hoveredState ifNil: [ ^ self ].
	hoverWidth := self indicatorWidth + 4.
	indicatorColor := (self colorForState: aSymbol) alpha: 0.8.
	indicatorBounds := self bounds left + self horizontalGapBefore @ (aTextLine top + 2) corner:
		                   self bounds left + self horizontalGapBefore + hoverWidth @ (aTextLine bottom - 2).
	indicatorBounds := indicatorBounds translateBy: 0 @ (self offset y negated + self bounds top).
	aCanvas setShape: indicatorBounds.
	aCanvas setPaint: indicatorColor.
	aCanvas draw
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawHoveredRemovedArrowAtY: yCoord on: aCanvas [

	| color arrowSize leftX topY botY off |

	hoveredState ifNil: [ ^ self ].
	color := (self colorForState: #removed) alpha: 0.8.
	arrowSize := self indicatorWidth + 4.
	leftX := self bounds left + self horizontalGapBefore.
	off := self offset y negated + self bounds top.
	topY := yCoord - (arrowSize // 2) + off.
	botY := yCoord + (arrowSize // 2) + off.

	aCanvas
		drawPolygon: {
			(leftX @ topY).
			(leftX + arrowSize @ (yCoord + off)).
			(leftX @ botY)
		}
		fillStyle: color
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawHoveredRemovedArrowAtY: yCoord onAthens: aCanvas [

	| color arrowSize leftX topY botY off |

	hoveredState ifNil: [ ^ self ].
	color := (self colorForState: #removed) alpha: 0.8.
	arrowSize := self indicatorWidth + 4.
	leftX := self bounds left + self horizontalGapBefore.
	off := self offset y negated + self bounds top.
	topY := yCoord - (arrowSize // 2) + off.
	botY := yCoord + (arrowSize // 2) + off.
	aCanvas setShape: (aCanvas createPath: [ :builder |
				 builder absolute.
				 builder moveTo: leftX @ topY.
				 builder lineTo: leftX + arrowSize @ (yCoord + off).
				 builder lineTo: leftX @ botY.
				 builder close ]).
	aCanvas setPaint: color.
	aCanvas draw
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawIndicatorBarOn: aCanvas forState: aSymbol fromLine: firstLine toLine: lastLine [

	| indicatorColor indicatorBounds |
	indicatorColor := self colorForState: aSymbol.
	indicatorBounds := self bounds left + self horizontalGapBefore @ (firstLine top + 2) corner:
		                self bounds left + self horizontalGapBefore + self indicatorWidth @ (lastLine bottom - 2).
	indicatorBounds := indicatorBounds translateBy: 0 @ (self offset y negated + self bounds top).
	aCanvas fillRectangle: indicatorBounds color: indicatorColor
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawIndicatorBarOnAthens: aCanvas forState: aSymbol fromLine: firstLine toLine: lastLine [

	| indicatorColor indicatorBounds |
	indicatorColor := self colorForState: aSymbol.
	indicatorBounds := self bounds left + self horizontalGapBefore @ (firstLine top + 2) corner:
		                self bounds left + self horizontalGapBefore + self indicatorWidth @ (lastLine bottom - 2).
	indicatorBounds := indicatorBounds translateBy: 0 @ (self offset y negated + self bounds top).
	aCanvas setShape: indicatorBounds.
	aCanvas setPaint: indicatorColor.
	aCanvas draw
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawOn: aCanvas [

	| lines visibleRectangle firstLine lastLine i |
	super drawOn: aCanvas.
	self paragraph ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	lineStates ifNil: [ ^ self ].
	lines := self lines.

	aCanvas clipBy: self bounds during: [ :clippedCanvas |
			clippedCanvas fillRectangle: self verticalSeparatorBounds color: self verticalSeparatorColor.
			visibleRectangle := clippedCanvas clipRect.
			firstLine := self lineIndexForPoint: visibleRectangle topLeft.
			lastLine := self lineIndexForPoint: visibleRectangle bottomRight.
			i := firstLine.
			[ i <= ((lastLine min: lineStates size) min: lines size) ] whileTrue: [
					| state |
					state := lineStates at: i.
					state = #unchanged
						ifTrue: [ i := i + 1 ]
						ifFalse: [
								| runStart |
								runStart := i.
								[ i < lineStates size and: [ (lineStates at: i + 1) = state ] ] whileTrue: [ i := i + 1 ].
								self
									drawIndicatorBarOn: clippedCanvas
									forState: state
									fromLine: (lines at: runStart)
									toLine: (lines at: i).
								i := i + 1 ] ].
			self consolidatedRemovedPositions do: [ :pos |
				(self gapYForPosition: pos lines: lines) ifNotNil: [ :y | self drawRemovedArrowAtY: y on: clippedCanvas ] ].
			hoveredState ifNotNil: [
					hoveredState first = #removed
						ifTrue: [
							(self gapYForPosition: hoveredState second lines: lines) ifNotNil: [ :y |
								self drawHoveredRemovedArrowAtY: y on: clippedCanvas ] ]
						ifFalse: [
								(hoveredState second between: firstLine and: lastLine) ifTrue: [
										hoveredState second <= lines size ifTrue: [
											self drawHoveredIndicatorBarOn: clippedCanvas forState: hoveredState first atLine: (lines at: hoveredState second) ] ] ] ] ]
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawOnAthensCanvas: aCanvas [

	| lines visibleRectangle firstLine lastLine i |
	super drawOnAthensCanvas: aCanvas.
	self paragraph ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	lineStates ifNil: [ ^ self ].
	lines := self lines.
	aCanvas clipBy: self bounds during: [
			aCanvas setShape: self verticalSeparatorBounds.
			aCanvas setPaint: self verticalSeparatorColor.
			aCanvas draw.
			visibleRectangle := aCanvas clipRect.
			firstLine := self lineIndexForPoint: visibleRectangle topLeft.
			lastLine := self lineIndexForPoint: visibleRectangle bottomRight.
			i := firstLine.
			[ i <= ((lastLine min: lineStates size) min: lines size) ] whileTrue: [
					| state |
					state := lineStates at: i.
					state = #unchanged
						ifTrue: [ i := i + 1 ]
						ifFalse: [
								| runStart |
								runStart := i.
								[ i < lineStates size and: [ (lineStates at: i + 1) = state ] ] whileTrue: [ i := i + 1 ].
								self
									drawIndicatorBarOnAthens: aCanvas
									forState: state
									fromLine: (lines at: runStart)
									toLine: (lines at: i).
								i := i + 1 ] ].
			self consolidatedRemovedPositions do: [ :pos |
				(self gapYForPosition: pos lines: lines) ifNotNil: [ :y | self drawRemovedArrowAtY: y onAthens: aCanvas ] ].
			hoveredState ifNotNil: [
					hoveredState first = #removed
						ifTrue: [
							(self gapYForPosition: hoveredState second lines: lines) ifNotNil: [ :y |
								self drawHoveredRemovedArrowAtY: y onAthens: aCanvas ] ]
						ifFalse: [
								(hoveredState second between: firstLine and: lastLine) ifTrue: [
										hoveredState second <= lines size ifTrue: [
											self drawHoveredIndicatorBarOnAthens: aCanvas forState: hoveredState first atLine: (lines at: hoveredState second) ] ] ] ] ]
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawRemovedArrowAtY: yCoord on: aCanvas [

	| color arrowSize leftX topY botY off |
	color := self colorForState: #removed.
	arrowSize := self indicatorWidth.
	leftX := self bounds left + self horizontalGapBefore.
	off := self offset y negated + self bounds top.
	topY := yCoord - (arrowSize // 2) + off.
	botY := yCoord + (arrowSize // 2) + off.
	aCanvas
		drawPolygon: {
				(leftX @ topY).
				(leftX + arrowSize @ (yCoord + off)).
				(leftX @ botY) }
		fillStyle: color
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> drawRemovedArrowAtY: yCoord onAthens: aCanvas [

	| color arrowSize leftX topY botY off |
	color := self colorForState: #removed.
	arrowSize := self indicatorWidth.
	leftX := self bounds left + self horizontalGapBefore.
	off := self offset y negated + self bounds top.
	topY := yCoord - (arrowSize // 2) + off.
	botY := yCoord + (arrowSize // 2) + off.
	aCanvas setShape: (aCanvas createPath: [ :builder |
				 builder absolute.
				 builder moveTo: leftX @ topY.
				 builder lineTo: leftX + arrowSize @ (yCoord + off).
				 builder lineTo: leftX @ botY.
				 builder close ]).
	aCanvas setPaint: color.
	aCanvas draw
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> editingMethod [

	^ editingMethod
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> editingMethod: aCompiledMethod [

	editingMethod := aCompiledMethod
]

{ #category : 'private' }
RubGutterDiffDisplayer >> findRemovalGapNearY: textY [
	"Return the removed position whose gap is closest to textY, or nil if none is close enough."

	self consolidatedRemovedPositions do: [ :pos |
			(self gapYForPosition: pos lines: self lines) ifNotNil: [ :gapY |
					(textY - gapY) abs <= self indicatorWidth ifTrue: [
							^ {
								  pos.
								  gapY } ] ] ].
	^ nil
]

{ #category : 'private' }
RubGutterDiffDisplayer >> flushDiffRunSrcStart: srcStart srcEnd: srcEnd dstStart: dstStart dstEnd: dstEnd srcLine: srcLine dstLine: dstLine [
	"Flush a collected run of patch entries. Based on which sides have content:
	- source only → #removal gap at dstLine+1
	- destination only → #added lines in [dstStart..dstEnd]
	- both → #modified lines in [dstStart..dstEnd]"

	| hasSrc hasDst |
	hasSrc := srcStart > 0.
	hasDst := dstStart > 0.
	hasSrc ifFalse: [
			hasDst ifTrue: [ dstStart to: dstEnd do: [ :i | lineStates at: i put: #added ] ].
			^ self ].

	hasDst ifFalse: [ removedPositions add: dstLine + 1 . ^ self ].

	dstStart to: dstEnd do: [ :i | lineStates at: i put: #modified ]
]

{ #category : 'private' }
RubGutterDiffDisplayer >> forwardMouseDownEventToTextArea: anEvent [

	self adjustEventPosition: anEvent.

	self textArea mouseDown: anEvent
]

{ #category : 'private' }
RubGutterDiffDisplayer >> forwardMouseMoveEventToTextArea: anEvent [

	self adjustEventPosition: anEvent.
	self textArea mouseMove: anEvent
]

{ #category : 'private' }
RubGutterDiffDisplayer >> forwardMouseUpEventToTextArea: anEvent [

	self adjustEventPosition: anEvent.
	self textArea mouseUp: anEvent
]

{ #category : 'drawing' }
RubGutterDiffDisplayer >> gapYForPosition: pos lines: lines [

	pos = 1 ifTrue: [ ^ lines ifNotEmpty: [ lines first top - 2 ] ifEmpty: [ nil ] ].
	
	pos > lines size ifTrue: [
		^ lines ifNotEmpty: [ lines last bottom + 2 ] ifEmpty: [ nil ]
	].

	^ (lines at: pos - 1) bottom + (lines at: pos) top // 2
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> handlesMouseDown: evt [

	^ true
]

{ #category : 'geometry' }
RubGutterDiffDisplayer >> horizontalGapAfter [

	^ 2
]

{ #category : 'geometry' }
RubGutterDiffDisplayer >> horizontalGapBefore [

	^ 2
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> hoveredState [

	^ hoveredState
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> hoveredState: anObject [

	hoveredState := anObject
]

{ #category : 'geometry' }
RubGutterDiffDisplayer >> indicatorWidth [

	^ 8
]

{ #category : 'initialization' }
RubGutterDiffDisplayer >> initialize [

	super initialize.
	removedPositions := OrderedCollection new.
	hoveredState := nil
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> level [

	^ 3
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> lineStates [

	^ lineStates
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> mouseDown: anEvent [

	| lineIndex state |
	(anEvent yellowButtonPressed and: [ anEvent commandKeyPressed not ]) ifTrue: [ ^ self yellowButtonActivity: anEvent shiftPressed ].
	lineStates ifNil: [ ^ self ].
	lineIndex := self lineIndexForPoint: anEvent position.
	lineIndex > lineStates size ifTrue: [ ^ self ].
	(self tryShowPreviewForRemovalAt: anEvent position) ifTrue: [ ^ self ].
	state := lineStates at: lineIndex.
	state = #unchanged ifTrue: [ ^ self forwardMouseDownEventToTextArea: anEvent ].
	self showPreviewAtLine: lineIndex
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> mouseLeave: anEvent [

	hoveredState := nil.
	self changed
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> mouseMove: anEvent [

	| newHoveredState |
	newHoveredState := self computeHoveredStateAt: anEvent position.
	newHoveredState ~= hoveredState ifTrue: [
			hoveredState := newHoveredState.
			self changed ].
	self forwardMouseMoveEventToTextArea: anEvent
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> mouseUp: anEvent [

	self forwardMouseUpEventToTextArea: anEvent
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> previewMorph [

	^ previewMorph
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> previewMorph: aRubGutterDiffPreviewMorph [

	previewMorph := aRubGutterDiffPreviewMorph
]

{ #category : 'refreshing' }
RubGutterDiffDisplayer >> refreshBaseline [

	baselineProvider ifNil: [ ^ self ].
	editingMethod ifNil: [ ^ self ].
	baseline := baselineProvider baselineForMethod: editingMethod ifNone: [ self textArea text asString ].
	self computeLineStates.
	self changed
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> removedPositions [

	^ removedPositions
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> scrollerOffsetChanged [

	super scrollerOffsetChanged.
	previewMorph ifNotNil: [
		previewMorph delete.
		previewMorph := nil
	]
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> selectionChanged [

	self changed
]

{ #category : 'private' }
RubGutterDiffDisplayer >> showPreviewAtLine: lineIndex [

	| currentText preview previewWidth worldBounds previewPos lineRect |
	previewMorph ifNotNil: [
			previewMorph delete.
			previewMorph := nil ].
	currentText := self textArea text asString.
	previewWidth := self textArea width.
	preview := RubGutterDiffPreviewMorph
		           forLine: lineIndex
		           baseline: (baseline ifNil: [ currentText ])
		           current: currentText
		           context: 3
		           width: previewWidth.
	preview openInWorld.
	worldBounds := self world ifNotNil: [ :w | w bounds ] ifNil: [ 0 @ 0 corner: 2000 @ 2000 ].
	lineRect := self lines at: lineIndex ifAbsent: [ self lines last ].
	previewPos := self bounds right + 5 @ (lineRect top - self offset y + self bounds top).
	previewPos x + preview width > worldBounds right ifTrue: [ previewPos := worldBounds right - preview width - 5 @ previewPos y ].
	previewPos y + preview height > worldBounds bottom ifTrue: [
		previewPos := previewPos x @ (worldBounds bottom - preview height - 5) ].
	preview position: previewPos.
	previewMorph := preview
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> showPreviewAtRemovedPosition: pos atY: textY [

	| currentText preview previewWidth worldBounds previewPos |
	previewMorph ifNotNil: [
			previewMorph delete.
			previewMorph := nil ].
	currentText := self textArea text asString.
	previewWidth := self textArea width.
	preview := RubGutterDiffPreviewMorph
		           forLine: pos
		           baseline: (baseline ifNil: [ currentText ])
		           current: currentText
		           context: 3
		           width: previewWidth.
	preview openInWorld.
	worldBounds := self world ifNotNil: [ :w | w bounds ] ifNil: [ 0 @ 0 corner: 2000 @ 2000 ].
	previewPos := self bounds right + 5 @ (textY - self offset y + self bounds top - (preview height // 2)).
	previewPos x + preview width > worldBounds right ifTrue: [ previewPos := worldBounds right - preview width - 5 @ previewPos y ].
	previewPos y + preview height > worldBounds bottom ifTrue: [
		previewPos := previewPos x @ (worldBounds bottom - preview height - 5) ].
	previewPos y < worldBounds top ifTrue: [ previewPos := previewPos x @ worldBounds top ].
	preview position: previewPos.
	previewMorph := preview
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> textChanged [

	self computeLineStates.
	super textChanged
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> tryShowPreviewForRemovalAt: aPoint [

	| textY result |
	lineStates ifNil: [ ^ false ].
	textY := aPoint y + self offset y - self bounds top.
	result := self findRemovalGapNearY: textY.
	result ifNil: [ ^ false ].
	self showPreviewAtRemovedPosition: result first atY: result second.
	^ true
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> verticalSeparatorBounds [

	| bnds |
	bnds := self bounds.
	^ self side = #left
		  ifTrue: [ bnds topRight - (self verticalSeparatorWidth @ 0) corner: bnds bottomRight ]
		  ifFalse: [ bnds topLeft corner: bnds bottomLeft + (self verticalSeparatorWidth @ 0) ]
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> verticalSeparatorColor [

	^ self textArea backgroundColor contrastingBlackAndWhiteColor alpha: 0.3
]

{ #category : 'accessing' }
RubGutterDiffDisplayer >> verticalSeparatorWidth [

	^ 1
]

{ #category : 'event handling' }
RubGutterDiffDisplayer >> yellowButtonActivity: shiftKeyState [

	^ true
]
