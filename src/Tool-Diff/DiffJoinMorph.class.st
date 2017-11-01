Class {
	#name : #DiffJoinMorph,
	#superclass : #ProportionalSplitterMorph,
	#instVars : [
		'srcOffset',
		'dstOffset',
		'mappings'
	],
	#category : 'Tool-Diff'
}

{ #category : #'accessing - defaults' }
DiffJoinMorph class >> splitterWidth [
	"Answer the width of splitter to use for dif joins."
	
	^7
]

{ #category : #actions }
DiffJoinMorph >> compositeText [
	"Answer the composite text based on the selection state
	of the joins."

	|t|
	t := Text new.
	self mappings do: [:j |
		j appendToCompositeText: t].
	^t
]

{ #category : #initialize }
DiffJoinMorph >> defaultColor [
	"Answer the default color for the receiver."

	^Color transparent
]

{ #category : #drawing }
DiffJoinMorph >> drawOn: aCanvas [
	"Draw the indicators for the mappings."
	
	super drawOn: aCanvas.
	aCanvas translateBy: self topLeft clippingTo: self clippingBounds during: [:c |
		self mappings do: [:j | j drawOn: c]]
]

{ #category : #drawing }
DiffJoinMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Draw the indicators for the mappings."

	super drawOnAthensCanvas: anAthensCanvas. 
	(anAthensCanvas asCanvasWrapper ) translateBy: self topLeft clippingTo: self clippingBounds during: [:c |
		self mappings do: [:j | j drawOn: c]]
]

{ #category : #accessing }
DiffJoinMorph >> dstOffset [
	"Answer the value of dstOffset"

	^ dstOffset
]

{ #category : #accessing }
DiffJoinMorph >> dstOffset: anInteger [
	"Set the dstOffset."

	dstOffset := anInteger.
	self mappings do: [:j |
		j dstOffset: anInteger]
]

{ #category : #geometry }
DiffJoinMorph >> extent: aPoint [
	"Update the shapes of the joins."
	
	super extent: aPoint.
	self updateMappings
]

{ #category : #initialization }
DiffJoinMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		mappings: OrderedCollection new;
		srcOffset: 0@0;
		dstOffset: 0@0
]

{ #category : #layout }
DiffJoinMorph >> layoutBounds: aRectangle [
	"Set the bounds for laying out children of the receiver."
	
	super layoutBounds: aRectangle.
	self updateMappings
]

{ #category : #accessing }
DiffJoinMorph >> mappings [
	"Answer the value of mappings"

	^ mappings
]

{ #category : #accessing }
DiffJoinMorph >> mappings: anObject [
	"Set the value of mappings"

	mappings := anObject.
	self updateMappings.
	self changed
]

{ #category : #'event handling' }
DiffJoinMorph >> mouseDown: evt [
	"Check for a click."

	self mappings
		detect: [ :j | j containsPoint: evt position - self topLeft ]
		ifFound: [ :cj | 
			cj clicked.
			self triggerEvent: #joinClicked ].
	super mouseDown: evt
]

{ #category : #accessing }
DiffJoinMorph >> srcOffset [
	"Answer the value of srcOffset"

	^ srcOffset
]

{ #category : #accessing }
DiffJoinMorph >> srcOffset: anInteger [
	"Set the srcOffset."

	srcOffset := anInteger.
	self mappings do: [:j |
		j srcOffset: anInteger]
]

{ #category : #'private - updating' }
DiffJoinMorph >> updateMappings [
	"Update the shapes of the joins."
	
	self mappings do: [:j |
		j width: self width]
]
