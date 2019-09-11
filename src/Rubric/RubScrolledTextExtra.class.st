"
RubScrolledTextExtra represents the root of rulers graphical representations (which add specific behavior to the scrolledText on which they are attached).

Subinstances are the graphical representation of the associated decorators defined in the .

 the main methods are 
	- computedExtentFrom:
	- computedHeightFrom: 
	- computedPositionFrom:
	- computedWidthFrom:
	

"
Class {
	#name : #RubScrolledTextExtra,
	#superclass : #Morph,
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubScrolledTextExtra class >> classOfRulerNamed: aKey [ 
	^ self allSubclasses detect: [ :cls | cls key = aKey ] ifNone: [  ]
]

{ #category : #querying }
RubScrolledTextExtra class >> key [
	^ nil
]

{ #category : #accessing }
RubScrolledTextExtra >> computedExtentFrom: aRectangle [
	^ (self computedWidthFrom: aRectangle) @ (self computedHeightFrom: aRectangle)
]

{ #category : #accessing }
RubScrolledTextExtra >> computedHeightFrom: aRectangle [
	^ aRectangle height
]

{ #category : #accessing }
RubScrolledTextExtra >> computedPositionFrom: aRectangle [
	^ aRectangle topLeft
]

{ #category : #accessing }
RubScrolledTextExtra >> computedWidthFrom: aRectangle [
	^ aRectangle width 
]

{ #category : #defaults }
RubScrolledTextExtra >> defaultBackgroundColor [ 
	^ Color transparent
]

{ #category : #defaults }
RubScrolledTextExtra >> defaultColor [
	^ self defaultBackgroundColor 
]

{ #category : #initialization }
RubScrolledTextExtra >> initialize [
	super initialize.
	self clipSubmorphs: true
]

{ #category : #testing }
RubScrolledTextExtra >> isHorizontal [
	^ false
]

{ #category : #testing }
RubScrolledTextExtra >> isOver [
	^ false
]

{ #category : #testing }
RubScrolledTextExtra >> isSideRuler [
	^ false
]

{ #category : #testing }
RubScrolledTextExtra >> isVertical [
	^ false
]

{ #category : #accessing }
RubScrolledTextExtra >> key [
	^self class key
]

{ #category : #accessing }
RubScrolledTextExtra >> level [
	^ 0
]

{ #category : #accessing }
RubScrolledTextExtra >> lineIndexForPoint: aPoint [
	^ self paragraph lineIndexForPoint: aPoint  + (0 @ (self offset y - self bounds top))
]

{ #category : #accessing }
RubScrolledTextExtra >> lines [
	^ self paragraph lines
]

{ #category : #geometry }
RubScrolledTextExtra >> manageLayoutInBounds: aRectangle [

]

{ #category : #accessing }
RubScrolledTextExtra >> margins [
	^ self textArea margins
]

{ #category : #accessing }
RubScrolledTextExtra >> offset [
	^ self paragraphProvider scrollPane offset
]

{ #category : #accessing }
RubScrolledTextExtra >> paragraph [
	^ self paragraphProvider ifNotNil: [:p | p paragraph]
]

{ #category : #accessing }
RubScrolledTextExtra >> paragraphProvider [
	
	^ self ownerThatIsA: RubScrolledTextMorph 
]

{ #category : #accessing }
RubScrolledTextExtra >> primarySelectionColor [
	^ self textArea primarySelectionColor
]

{ #category : #accessing }
RubScrolledTextExtra >> scrollBounds [
	^ self paragraphProvider scrollBounds
]

{ #category : #accessing }
RubScrolledTextExtra >> scrollPane [
	^ self paragraphProvider scrollPane
]

{ #category : #'event handling' }
RubScrolledTextExtra >> scrollerOffsetChanged [
	self changed
]

{ #category : #'event handling' }
RubScrolledTextExtra >> selectionChanged [
	self changed
]

{ #category : #accessing }
RubScrolledTextExtra >> selectionStart [
	^ self paragraph selectionStart
]

{ #category : #accessing }
RubScrolledTextExtra >> selectionStop [
	^ self paragraph selectionStop
]

{ #category : #accessing }
RubScrolledTextExtra >> text [ 
	^ self textArea text
]

{ #category : #'event handling' }
RubScrolledTextExtra >> textAccepted [
	self changed
]

{ #category : #accessing }
RubScrolledTextExtra >> textArea [
	^ self paragraph textArea
]

{ #category : #'event handling' }
RubScrolledTextExtra >> textChanged [
	self changed
]

{ #category : #accessing }
RubScrolledTextExtra >> textStyle [

	^ self textArea textStyle.

]

{ #category : #structure }
RubScrolledTextExtra >> topRendererOrSelf [
	^self paragraphProvider
]

{ #category : #initialization }
RubScrolledTextExtra >> unplug [
	self paragraphProvider
		ifNotNil: [ 
			self scrollPane ifNotNil: [:sp | sp announcer unsubscribe: self].
			self textArea ifNotNil: [:ts | ts announcer unsubscribe: self ] ].
	super unplug 
]
