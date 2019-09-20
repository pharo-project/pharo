"
A Morph that views another morph, its objectToView.
"
Class {
	#name : #ThumbnailMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'objectToView',
		'viewSelector',
		'lastSketchForm',
		'lastFormShown',
		'drawTime'
	],
	#classVars : [
		'EccentricityThreshhold',
		'RecursionDepth',
		'RecursionMax'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #example }
ThumbnailMorph class >> example1 [
	"Create a thumbnail representing another Morph. The thumbnail is continously updated."
	"self example1"
	| t r |
	r := Morph new.
	r position: 100@200.
	t := ThumbnailMorph new objectToView: r viewSelector: #openInWorld.
	t openInWorld
]

{ #category : #initialization }
ThumbnailMorph class >> initialize [
	"Initialize the class variables of ThumbnailMorph"

	RecursionMax := 2.
	RecursionDepth := 0.
	EccentricityThreshhold :=  Float pi

"ThumbnailMorph initialize"
]

{ #category : #utilities }
ThumbnailMorph class >> recursionReset [
	"ThumbnailMorph recursionReset"
	"Reset the RecursionDepth counter in case the user interrupted
during a thumbnail being drawn.  Do this just once in a while when no
drawOn: is being called.  tk 9/8/97"

	RecursionDepth := 0.
]

{ #category : #'what to view' }
ThumbnailMorph >> actualViewee [
	"Return the actual morph to be viewed, or nil if there isn't an appropriate morph to view."

	| aMorph actualViewee |
	aMorph := self morphToView ifNil: [^ nil]. 
	aMorph isInWorld ifFalse: [^ nil].
	actualViewee := viewSelector ifNil: [aMorph] ifNotNil: [objectToView perform: viewSelector].
	actualViewee = 0 ifTrue: [^ nil].  
	actualViewee ifNil: [actualViewee := objectToView].
	(actualViewee isMorph and: 
		[actualViewee isFlexMorph and: [actualViewee submorphs size = 1]])
			ifTrue: [actualViewee := actualViewee firstSubmorph].
	^ actualViewee
]

{ #category : #initialization }
ThumbnailMorph >> defaultBorderWidth [
"answer the default border width for the receiver"
	^ 1
]

{ #category : #initialization }
ThumbnailMorph >> defaultColor [
"answer the default color/fill style for the receiver"
	^ Color
		r: 0.781
		g: 0.781
		b: 0.781
]

{ #category : #display }
ThumbnailMorph >> drawForForm: aForm on: aCanvas [
	"Draw a small view of the given form on the canvas"

	| scale shrunkForm viewedObjectBox interimCanvas |
	viewedObjectBox := aForm boundingBox.
	scale :=  self scaleFor: viewedObjectBox in: self innerBounds.
	interimCanvas := Display defaultCanvasClass extent: viewedObjectBox extent depth: aCanvas depth.
	interimCanvas translateBy: viewedObjectBox topLeft negated 
				during: [:tempCanvas | tempCanvas drawImage: aForm at: 0@0].
	shrunkForm := interimCanvas form magnify: interimCanvas form boundingBox by: scale smoothing: 1.
	lastFormShown := shrunkForm.

	aCanvas paintImage: shrunkForm at: self center - shrunkForm boundingBox center
]

{ #category : #display }
ThumbnailMorph >> drawMeOn: aCanvas [ 
	"Draw a small view of a morph in another place.  Guard against infinite recursion if that morph has a thumbnail of itself inside.  Now also works if the thing to draw is a plain Form rather than a morph."

	| viewedMorphBox scale c shrunkForm aWorld aFormOrMorph  |
	super drawOn: aCanvas.
	((aFormOrMorph := self formOrMorphToView) isForm) 
		ifTrue: [^self drawForForm: aFormOrMorph on: aCanvas].
	(((aFormOrMorph notNil and: [(aWorld := aFormOrMorph world) notNil]) 
		and: [aWorld ~~ aFormOrMorph or: [lastFormShown isNil]]) 
			and: [RecursionDepth + 1 < RecursionMax]) 
			ifTrue: 
				[RecursionDepth := RecursionDepth + 1.
				viewedMorphBox := aFormOrMorph fullBounds.
			scale :=  self scaleFor: viewedMorphBox in: self innerBounds.
				c := Display defaultCanvasClass extent: viewedMorphBox extent
							depth: aCanvas depth.
				c translateBy: viewedMorphBox topLeft negated
					during: 
						[:tempCanvas | 
						"recursion happens here"
						tempCanvas fullDrawMorph: aFormOrMorph].
				shrunkForm := c form 
							magnify: c form boundingBox
							by: scale
							smoothing: 1.
				lastFormShown := shrunkForm.
				RecursionDepth := RecursionDepth - 1]
			ifFalse: 
				["This branch used if we've recurred, or if the thumbnail views a World that's already been rendered once, or if the referent is not in a world at the moment"
				lastFormShown ifNotNil: [shrunkForm := lastFormShown]].
	shrunkForm ifNotNil: 
			[aCanvas paintImage: shrunkForm
				at: self center - shrunkForm boundingBox center]
]

{ #category : #drawing }
ThumbnailMorph >> drawOn: aCanvas [
	"Draw a small view of a morph in another place. Guard against infinite recursion if that morph has a thumbnail of itself inside."
	| time |
	time := Time millisecondClockValue.
	self drawMeOn: aCanvas.
	drawTime := Time millisecondClockValue - time.
	drawTime < 0 ifTrue:[drawTime := nil].

]

{ #category : #'what to view' }
ThumbnailMorph >> formOrMorphToView [
	"Answer the form to be viewed, or the morph to be viewed, or nil"

	| actualViewee |
	(objectToView isForm) ifTrue: [^objectToView].
	actualViewee := viewSelector ifNil: [objectToView]
				ifNotNil: [objectToView perform: viewSelector].
	^actualViewee = 0 
		ifTrue: [nil]
		ifFalse: [actualViewee]
]

{ #category : #accessing }
ThumbnailMorph >> getSelector [
	"Answer the selector I send to my target to retrieve my value"

	^ viewSelector
]

{ #category : #accessing }
ThumbnailMorph >> getSelector: aSelector [
	"Set the selector used to obtain my value"

	self objectToView: objectToView viewSelector: aSelector
]

{ #category : #initialization }
ThumbnailMorph >> initialize [
	"Initialize the receiver, obeying a #nominalExtent property if I have one"
	| anExtent |
	
	super initialize.
	anExtent := self valueOfProperty: #nominalExtent ifAbsent: [25@25].
	self extent: anExtent
]

{ #category : #'what to view' }
ThumbnailMorph >> morphToView [
	"If the receiver is viewing some object, answer a morph can be thought of as being viewed;  A gesture is made toward generalizing this beyond the morph/player regime, in that a plain blue rectangle is returned rather than simply failing if the referent is not itself displayable."

	objectToView ifNil: [^ nil].
	^ objectToView isMorph
		ifTrue:
			[objectToView]
		ifFalse:
			[Morph new color: Color blue]

]

{ #category : #initialization }
ThumbnailMorph >> objectToView: objectOrNil [
	(objectOrNil isMorph and: [objectOrNil allMorphs includes: self]) ifTrue:
		["cannot view a morph containing myself or drawOn: goes into infinite recursion"
		objectToView := nil.
		^ self].
	objectToView := objectOrNil
]

{ #category : #initialization }
ThumbnailMorph >> objectToView: objectOrNil viewSelector: aSelector [
	self objectToView: objectOrNil.
	viewSelector := aSelector
]

{ #category : #accessing }
ThumbnailMorph >> putSelector [
	"Answer the selector used  for the receiver to send a fresh value back to its target"

	^ nil
]

{ #category : #caching }
ThumbnailMorph >> releaseCachedState [
	super releaseCachedState.
	lastSketchForm := lastFormShown := nil.
]

{ #category : #display }
ThumbnailMorph >> scaleFor:  viewedMorphBox in: myBox [
	"Compute the proper scale for the thumbnail."

	|   scale  scaleX scaleY ratio factor  |
scaleX := myBox width asFloat / viewedMorphBox width.
				scaleY := myBox height asFloat / viewedMorphBox height.
				ratio := scaleX / scaleY.
				factor := 1.0 / EccentricityThreshhold.
				ratio < factor
					ifTrue:
						[scale := (scaleX) @ (factor * scaleY)]
					ifFalse:
						[ratio > EccentricityThreshhold
							ifTrue:
								[scale := (factor * scaleX) @ scaleY]
							ifFalse:
								[scale := scaleX min: scaleY]].
^ scale

]

{ #category : #stepping }
ThumbnailMorph >> step [
	"Optimization: Don't redraw if we're viewing some kind of SketchMorph and its rotated Form hasn't changed."

	| viewee |
	viewee := self actualViewee.
	viewee ifNil: [ self stopStepping. ^self ].
	self changed.

]

{ #category : #stepping }
ThumbnailMorph >> stepTime [ 
	"Adjust my step time to the time it takes drawing my referent"
	drawTime ifNil:[^ 250].
	^(20 * drawTime) max: 250.
]

{ #category : #accessing }
ThumbnailMorph >> target [
	"Answer the object on which I act"

	^ objectToView
]

{ #category : #copying }
ThumbnailMorph >> veryDeepFixupWith: deepCopier [
	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
objectToView := deepCopier references at: objectToView ifAbsent: [objectToView].
]

{ #category : #copying }
ThumbnailMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"objectToView := objectToView.		Weakly copied"
viewSelector := viewSelector veryDeepCopyWith: deepCopier.
lastSketchForm := lastSketchForm veryDeepCopyWith: deepCopier.
lastFormShown := lastFormShown veryDeepCopyWith: deepCopier.
drawTime := drawTime veryDeepCopyWith: deepCopier.

]
