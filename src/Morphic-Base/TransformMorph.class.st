"
A TransformMorph introduces a 2-D transformation between its (global) coordinates and the (local) coordinates of its submorphs, while also clipping all display to its bounds.  Specifically, with no offset, angle or scaling, a submorph with coordinates (0@0) will appear exactly at the topLeft of the windowMorph (its position).  Rotation and scaling are relative to the local origin, (0@0).

instance var	type				description
 transform		MorphicTransform	The coordinate transform between my coordinates and the
									local coordinates of my submorphs.
 smoothing		anInteger in 1..3	Perform smoothing of my contents during drawing
										1 No smoothing (#smoothingOff)
										2 Smoothing w/ edge adjacent pixels (#smoothingOn)
										3 Smoothing w/ edge and corner adj pixels
			
 localBounds	Rectangle or nil		caches the value of #localSubmorphBounds for performance

TransformMorphs operate with two different display strategies, depending on whether the transformation is a pure translation or not.  If so, then they simply use a clipping canvas and display their submorphs with the appropriate offset.  If the transformation includes scaling or rotation, then a caching canvas is used, whose active area covers the fullBounds of the submorphs intersected with the source quadrilateral corresponding to the window bounds.
"
Class {
	#name : #TransformMorph,
	#superclass : #Morph,
	#instVars : [
		'transform',
		'smoothing',
		'localBounds'
	],
	#category : 'Morphic-Base-Basic'
}

{ #category : #menu }
TransformMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	smoothing = 1
		ifTrue: [aCustomMenu add: 'turn on smoothing' selector: #smoothingOn]
		ifFalse: [aCustomMenu add: 'turn off smoothing' selector: #smoothingOff]
]

{ #category : #accessing }
TransformMorph >> angle [
	^ transform angle
]

{ #category : #accessing }
TransformMorph >> angle: newAngle [

	self changed.
	transform := transform withAngle: newAngle.
	self layoutChanged.
	self changed
]

{ #category : #'change reporting' }
TransformMorph >> areasRemainingToFill: aRectangle [

	^Array with: aRectangle
]

{ #category : #accessing }
TransformMorph >> clipSubmorphs [
	"Answer true since we do!
	This avoids the senseless over-invalidation
	with lists etc."
	
	^true
]

{ #category : #accessing }
TransformMorph >> colorForInsets [
	^ owner ifNil: [color] ifNotNil: [owner color]
]

{ #category : #'geometry testing' }
TransformMorph >> containsPoint: aPoint [
	(bounds containsPoint: aPoint) ifFalse: [^ false].
	self hasSubmorphs
		ifTrue: [ | localPoint |  localPoint := (transform globalPointToLocal: aPoint) .
				self submorphsDo: 
					[:m | (m containsPoint: localPoint)
							ifTrue: [^ true]].
				^ false]
		ifFalse: [^ true]
]

{ #category : #initialization }
TransformMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color lightGreen
]

{ #category : #accessing }
TransformMorph >> extent: aPoint [
	"Changed to not cause layoutChanged on the receiver."
	
	bounds extent = aPoint ifTrue: [^ self].
	self changed.
	bounds := (bounds topLeft extent: aPoint) rounded.
	super layoutChanged. "skip submorph bounds recalculation"
	self changed.

]

{ #category : #layout }
TransformMorph >> fullBounds [
	"Overridden to clip submorph hit detection to my bounds."
	"It might be better to override doLayoutIn:, and remove this method"

	fullBounds ifNotNil:[^ fullBounds].
	fullBounds := bounds.
	submorphs do: [:m| m ownerChanged].
	^ fullBounds
]

{ #category : #dropping/grabbing }
TransformMorph >> grabTransform [
	"Return the transform for the receiver which should be applied during grabbing"
	^owner ifNil:[self transform] ifNotNil:[owner grabTransform composedWithLocal: self transform]
]

{ #category : #initialization }
TransformMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	
	smoothing := 1.
	transform := MorphicTransform identity
]

{ #category : #'change reporting' }
TransformMorph >> invalidRect: damageRect from: aMorph [
	"Translate damage reports from submorphs by the scrollOffset."
	aMorph == self
		ifTrue:[super invalidRect: damageRect from: self]
		ifFalse:[super invalidRect: (((transform localBoundsToGlobal: damageRect) intersect: bounds ifNone: [ ^ self ] ) expandBy: 1) from: self].
]

{ #category : #geometry }
TransformMorph >> layoutChanged [

	"A submorph could have moved, thus changing my localBounds. Invalidate the cache."
	localBounds := nil.

	^super layoutChanged
]

{ #category : #geometry }
TransformMorph >> localSubmorphBounds [
	"Answer, in my coordinate system, the bounds of all my submorphs (or nil if no submorphs). We will cache this value for performance. The value is invalidated upon recieving #layoutChanged."

	localBounds ifNil:[
		self submorphsDo:[:m |
			localBounds ifNil: [localBounds := m fullBounds]
						ifNotNil: [localBounds := localBounds quickMerge: m fullBounds]].
	].	

	^ localBounds
]

{ #category : #geometry }
TransformMorph >> localVisibleSubmorphBounds [
	"Answer, in my coordinate system, the bounds of all my visible submorphs (or nil if no visible submorphs)"
	| subBounds |
	subBounds := nil.
	self submorphsDo: [:m |
		(m visible) ifTrue: [
			subBounds
				ifNil: [subBounds := m fullBounds copy]
				ifNotNil: [subBounds := subBounds quickMerge: m fullBounds]]
			].
	^subBounds
]

{ #category : #geometry }
TransformMorph >> numberOfItemsInView [
	"Answer the number of my submorphs whose (transformed) bounds intersect mine.
	This includes items that are only partially visible.
	Ignore visibility of submorphs."

	^(submorphs select: [ :ea | self innerBounds intersects: (transform localBoundsToGlobal: ea bounds) ]) size
]

{ #category : #geometry }
TransformMorph >> numberOfItemsPotentiallyInView [

	"Answer the number of items that could potentially be viewed in full,
	computed as my visible height divided by the average height of my submorphs.
	Ignore visibility of submorphs."

	^ self numberOfItemsPotentiallyInViewWith: self submorphCount
]

{ #category : #geometry }
TransformMorph >> numberOfItemsPotentiallyInViewWith: listSize [ 
	"This is a refactoring for numberOfItemsPotentiallyInView because in some cases the numberOfSubmorhps
	needs to be passed from the outside. "
	| height |
	height := self localSubmorphBounds height.
	height isZero ifTrue: [ ^ 0 ].
	^ self innerBounds height // (height / listSize)
]

{ #category : #accessing }
TransformMorph >> offset [
	^ transform offset + self innerBounds topLeft
]

{ #category : #accessing }
TransformMorph >> offset: newOffset [

	transform := transform withOffset: newOffset - self innerBounds topLeft.
	self changed
]

{ #category : #private }
TransformMorph >> privateFullMoveBy: delta [
	"Private! Relocate me, but not my subMorphs."

	self privateMoveBy: delta.
	transform :=  (transform asMorphicTransform) withOffset: (transform offset - delta).

]

{ #category : #accessing }
TransformMorph >> scale [
	^ transform scale
]

{ #category : #accessing }
TransformMorph >> scale: newScale [

	self changed.
	transform := transform withScale: newScale.
	self layoutChanged.
	self changed.

]

{ #category : #accessing }
TransformMorph >> setOffset: newOffset angle: newAngle scale: newScale [

	transform := MorphicTransform offset: newOffset angle: newAngle scale: newScale.
	self changed
]

{ #category : #accessing }
TransformMorph >> smoothing [
	^smoothing

]

{ #category : #accessing }
TransformMorph >> smoothing: cellSize [
	smoothing := cellSize.
	self changed
]

{ #category : #accessing }
TransformMorph >> smoothingOff [
	smoothing := 1.
	self changed
]

{ #category : #accessing }
TransformMorph >> smoothingOn [
	smoothing := 2.
	self changed
]

{ #category : #layout }
TransformMorph >> submorphBounds [
	"Answer, in owner coordinates, the bounds of my visible submorphs, or my bounds"
	^ (self localVisibleSubmorphBounds ifNotNil: [ :box |transform localBoundsToGlobal: box ] ifNil: [ self bounds ]) truncated.

]

{ #category : #accessing }
TransformMorph >> transform [
	^transform
]

{ #category : #accessing }
TransformMorph >> transform: aTransform [
	transform := aTransform.
]

{ #category : #'event handling' }
TransformMorph >> transformFrom: uberMorph [ 
	"Return a transform to map coorinates of uberMorph, a morph above me in my owner chain, into the coordinates of my submorphs."

	(self == uberMorph or: [owner isNil]) ifTrue: [^transform].
	^(owner transformFrom: uberMorph) composedWithLocal: transform
]

{ #category : #'halos and balloon help' }
TransformMorph >> wantsHaloFromClick [
	^ false
]
