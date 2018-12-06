Extension { #name : #ProportionalSplitterMorph }

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> adoptPaneColor: paneColor [
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	self fillStyle: self normalFillStyle
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> hasLeftOrTop: aMorph [
	"Answer whether the reciver has the given morph
	as one of of its left or top morphs."

	^leftOrTop includes: aMorph
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> hasRightOrBottom: aMorph [
	"Answer whether the reciver has the given morph
	as one of of its right or bottom morphs."

	^rightOrBottom includes: aMorph
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> hideLeftOrTop [
	"Hide the receiver and all left or top morphs."

	self hide.
	leftOrTop do: [:m | m hide]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> hideRightOrBottom [
	"Hide the receiver and all right or bottom morphs."

	self hide.
	rightOrBottom do: [:m | m hide]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> mouseUp: anEvent [
	"Change the cursor back to normal if necessary and change the color back to normal."
	
	(self bounds containsPoint: anEvent cursorPoint)
		ifFalse: [anEvent hand showTemporaryCursor: nil].
	self class fastSplitterResize
		ifTrue: [self updateFromEvent: anEvent].
	traceMorph ifNotNil: [traceMorph delete. traceMorph := nil].
	self adoptPaneColor: self paneColor.
	self triggerEvent: #mouseUp
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme splitterNormalFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> noteNewOwner: o [
	"Update the fill style."
	
	super noteNewOwner: o.
	self defer: [self adoptPaneColor: self paneColor]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> overlapsHorizontal: aSplitter [
	"Answer whether the receiver overlaps the given spiltter
	in the horizontal plane."

	^aSplitter left <= self right and: [aSplitter right >= self left]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> overlapsVertical: aSplitter [
	"Answer whether the receiver overlaps the given spiltter
	in the vertical plane."

	^aSplitter top <= self bottom and: [aSplitter bottom >= self top]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^self theme splitterPressedFillStyleFor: self
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> setGrabbedColor [
	"Set the color of the receiver when it is grabbed."
	
	self fillStyle: self pressedFillStyle
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> shouldDraw [
	"Answer whether the resizer should be drawn."
	
	^super shouldDraw or: [self class showSplitterHandles]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> shouldInvalidateOnMouseTransition [
	"Answer whether the resizer should be invalidated
	when the mouse enters or leaves."
	
	^self class showSplitterHandles
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> showLeftOrTop [
	"Show the receiver and all left or top morphs."

	self show.
	leftOrTop do: [:m | m show]
]

{ #category : #'*Polymorph-Widgets-Core' }
ProportionalSplitterMorph >> showRightOrBottom [
	"Show the receiver and all right or bottom morphs."

	self show.
	rightOrBottom do: [:m | m show]
]
