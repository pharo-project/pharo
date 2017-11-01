"
A TransformationMorph is like a transformMorph, except that it does not clip, and its bounds include its entire submorph.  TransformationMorphs are assumed to have only one submorph -- the idea is that it is a wrapper that enables its submorph to scale and rotate.  A TransformationMorph may come to have more than one submorph if, eg, a menu sprouts a sub menu, using the transformationMorph temporarily as its world, but this ability is only sparsely supported (as in layoutChanged).

See TransformationMorph class example1 method.
"
Class {
	#name : #TransformationMorph,
	#superclass : #TransformMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#category : 'Morphic-Base-Basic'
}

{ #category : #example }
TransformationMorph class >> example1 [
	| stringMorph transformationMorph |
	stringMorph := 'vertical text' asMorph.
	transformationMorph := TransformationMorph new asFlexOf: stringMorph.
	transformationMorph angle: Float pi / 2.
	transformationMorph position: 5@5.
	transformationMorph openInWorld.
]

{ #category : #private }
TransformationMorph >> adjustAfter: changeBlock [ 
	"Cause this morph to remain cetered where it was before, and
	choose appropriate smoothing, after a change of scale or rotation."

	changeBlock value.
	self chooseSmoothing.
	self layoutChanged.
	owner ifNotNil: [owner invalidRect: bounds]

]

{ #category : #initialization }
TransformationMorph >> asFlexOf: aMorph [
	"Initialize me with position and bounds of aMorph,
	and with an offset that provides centered rotation."
	| pos |
	pos := aMorph position.
	self addMorph: aMorph.
	aMorph position: (aMorph extent // 2) negated.
	self position: pos.
	transform := transform withOffset: aMorph position - pos

]

{ #category : #private }
TransformationMorph >> chooseSmoothing [
	"Choose appropriate smoothing, after a change of scale or rotation."

	smoothing := (self scale < 1.0 or: [self angle ~= (self angle roundTo: Float pi / 2.0)]) 
		ifTrue: [ 2]
		ifFalse: [1]
]

{ #category : #geometry }
TransformationMorph >> computeBounds [
	self hasSubmorphs ifTrue:
		[bounds := (transform localBoundsToGlobal:
					(Rectangle merging:
						(self submorphs collect: [:m | m fullBounds]))) truncated
				expandBy: 1].
	fullBounds := bounds.
]

{ #category : #'geometry etoy' }
TransformationMorph >> degreesOfFlex [
	"Return any rotation due to flexing"
	^ self rotationDegrees
]

{ #category : #drawing }
TransformationMorph >> drawOn: aCanvas [
	submorphs isEmpty ifTrue: [super drawOn: aCanvas]
]

{ #category : #accessing }
TransformationMorph >> embeddedWindowOrNil [

	"answer nil for common morphs, yourself from system windows and first submorph for transformation morphs"
	
	| s | 
	
	(self submorphs size = 1) ifTrue: [ 
		s := self firstSubmorph.
		s isSystemWindow ifTrue: [ ^ s ] ].
	^ nil.
]

{ #category : #geometry }
TransformationMorph >> extent: newExtent [
	
	self adjustAfter:
		[ | scaleFactor |scaleFactor := (self scale * newExtent r / self fullBounds extent r) max: 0.1.
		self scale: (scaleFactor detentBy: 0.1 atMultiplesOf: 1.0 snap: false)]
]

{ #category : #accessing }
TransformationMorph >> forwardDirection [
	"Return the rendee's forward direction. 
	If I have no rendee then return 0.0 degrees "
	| rendee |
	^ ( rendee := self renderedMorph) == self  
		ifTrue: [0.0 ]
		ifFalse: [^ rendee forwardDirection]
]

{ #category : #dropping/grabbing }
TransformationMorph >> grabTransform [
	"Return the transform for the receiver which should be applied during grabbing"
	self renderedMorph isWorldMorph 
		ifTrue:[^owner ifNil:[IdentityTransform new] ifNotNil:[owner grabTransform]].
	^owner ifNil:[self transform] ifNotNil:[owner grabTransform composedWithLocal: self transform]
]

{ #category : #accessing }
TransformationMorph >> hasNoScaleOrRotation [

	^ transform isPureTranslation

]

{ #category : #'geometry etoy' }
TransformationMorph >> heading [
	"End recusion when necessary."
	| rendee |
	^ (rendee := self renderedMorph) == self 
		ifTrue: [ 0.0 ]
		ifFalse: [rendee heading]
]

{ #category : #classification }
TransformationMorph >> isFlexMorph [

	^ true

]

{ #category : #classification }
TransformationMorph >> isRenderer [

	^ true

]

{ #category : #testing }
TransformationMorph >> isSticky [
	submorphs isEmpty ifFalse: [^ submorphs first isSticky].
	^false
]

{ #category : #layout }
TransformationMorph >> layoutChanged [
	"Recompute bounds as a result of change"
	self computeBounds.
	super layoutChanged
]

{ #category : #'rotate scale and flex' }
TransformationMorph >> prepareForRotating [

]

{ #category : #'rotate scale and flex' }
TransformationMorph >> prepareForScaling [

]

{ #category : #printing }
TransformationMorph >> printOn: aStream [ 
	super printOn: aStream.
	submorphs isEmpty 
		ifTrue: [aStream nextPutAll: ' with no transformee!']
		ifFalse: [aStream nextPutAll: ' on ' , submorphs first printString]
]

{ #category : #'geometry etoy' }
TransformationMorph >> referencePosition [
	"Answer the  receiver's reference position, bullet-proofed against infinite recursion in the unlikely but occasionally-seen case that I am my own renderee"

	| rendered |
	^ (rendered := self renderedMorph) == self
		ifTrue:
			[super referencePosition]
		ifFalse:
			[transform localPointToGlobal: rendered referencePosition]
]

{ #category : #menu }
TransformationMorph >> removeFlexShell [
	"Remove the shell used to make a morph rotatable and scalable."

	| oldHalo unflexed myWorld refPos aPosition |
	refPos := self referencePosition.
	myWorld := self world.
	oldHalo := self halo.
	submorphs isEmpty ifTrue: [^ self delete].
	aPosition := (owner submorphIndexOf: self) ifNil: [1].
	unflexed := self firstSubmorph.
	self submorphs do: [:m |
		m position: self center - (m extent // 2).
		owner addMorph: m asElementNumber: aPosition].
	oldHalo ifNotNil: [oldHalo setTarget: unflexed].
	myWorld ifNotNil: [myWorld startSteppingSubmorphsOf: unflexed].
	self delete.
	unflexed referencePosition: refPos.
	^ unflexed
]

{ #category : #classification }
TransformationMorph >> renderedMorph [
"We are a renderer. Answer appropriately."

	submorphs isEmpty ifTrue: [^self].
	^self firstSubmorph renderedMorph
]

{ #category : #submorphs-add/remove }
TransformationMorph >> replaceSubmorph: oldMorph by: newMorph [
	| t b |
	t := transform.
	b := bounds.
	super replaceSubmorph: oldMorph by: newMorph.
	transform := t.
	bounds := b.
	self layoutChanged
]

{ #category : #'rotate scale and flex' }
TransformationMorph >> rotationDegrees [
	^ self angle radiansToDegrees negated
]

{ #category : #accessing }
TransformationMorph >> rotationDegrees: degrees [
	self adjustAfter:[self angle: degrees degreesToRadians negated]
]

{ #category : #accessing }
TransformationMorph >> scaleFactor [
	"Answer the scaleFactor"

	^ transform scale
]

{ #category : #nil }
TransformationMorph >> scaleToMatch: aPoint [
	| scaleFactor tfm originalScale |
	tfm := transform withScale: 1.0.
	originalScale := ((tfm localBoundsToGlobal: self renderedMorph fullBounds) corner -
		(tfm localPointToGlobal: self renderedMorph referencePosition)) r.
	"Catch cases where the reference point is on fullBounds corner"
	originalScale < 1.0 ifTrue:[originalScale := 1.0].
	scaleFactor := (aPoint - self referencePosition) r / originalScale.
	scaleFactor := scaleFactor < 1.0
		ifTrue: [scaleFactor detentBy: 0.05 atMultiplesOf: 0.25 snap: false]
		ifFalse: [scaleFactor detentBy: 0.1 atMultiplesOf: 0.5 snap: false].
	self adjustAfter:[self scale: ((scaleFactor min: 8.0) max: 0.1)].
]

{ #category : #'geometry etoy' }
TransformationMorph >> setDirectionFrom: aPoint [
	| delta degrees inner |
	inner := self renderedMorph.
	inner == self ifTrue:[^self].
	delta := (inner transformFromWorld globalPointToLocal: aPoint) - inner referencePosition.
	degrees := delta degrees + 90.0.
	self forwardDirection: (degrees \\ 360) rounded.

]

{ #category : #testing }
TransformationMorph >> stepTime [
	"Answer the stepTime of my rendered morph if posible"

	| rendered |
	rendered := self renderedMorph.
	rendered = self ifTrue: [^super stepTime].	"Hack to avoid infinite recursion"
	^rendered stepTime.
	
]

{ #category : #geometry }
TransformationMorph >> transformedBy: aTransform [
	self changed.
	self transform: (self transform composedWithGlobal: aTransform).
	self computeBounds.
	self changed.
]

{ #category : #'geometry etoy' }
TransformationMorph >> visible: aBoolean [
	"Set the receiver's visibility property"

	super visible: aBoolean.
	submorphs isEmptyOrNil ifFalse: [submorphs first visible: aBoolean]
]
