"
I represent a Form obtained by replicating a pattern form indefinitely in all directions.
"
Class {
	#name : #InfiniteForm,
	#superclass : #DisplayObject,
	#instVars : [
		'patternForm'
	],
	#category : #'Graphics-Display Objects-Forms'
}

{ #category : #'instance creation' }
InfiniteForm class >> with: aForm [ 
	"Answer an instance of me whose pattern form is the argument, aForm."

	^self new 
		form: aForm;
		yourself
]

{ #category : #adding }
InfiniteForm >> addFillStyleMenuItems: aMenu hand: aHand from: aMorph [
	"Add the items for changing the current fill style of the receiver"

	"prevents a walkback when control menu is built for morph with me as color"
]

{ #category : #accessing }
InfiniteForm >> asColor [
	^ patternForm dominantColor
]

{ #category : #accessing }
InfiniteForm >> asForm [
	^ patternForm
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> bitPatternForDepth: suspectedDepth [
	^ patternForm
]

{ #category : #displaying }
InfiniteForm >> colorForInsets [
	^ Color transparent
]

{ #category : #'display box access' }
InfiniteForm >> computeBoundingBox [ 
	"Refer to the comment in DisplayObject|computeBoundingBox."

	^0 @ 0 corner: SmallInteger maxVal @ SmallInteger maxVal
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> direction [
	^patternForm width @ 0
]

{ #category : #displaying }
InfiniteForm >> displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm [
	"This is the real display message, but it doesn't get used until the new
	display protocol is installed."
	| targetBox patternBox bb |
	(patternForm isForm) ifFalse:
		[^ aDisplayMedium fill: clipRectangle rule: ruleInteger fillColor: patternForm].

	"Do it iteratively"
	targetBox := aDisplayMedium boundingBox intersect: clipRectangle ifNone: [ ^ self ].
	patternBox := patternForm boundingBox.
	bb := BitBlt destForm: aDisplayMedium sourceForm: patternForm fillColor: aForm
		combinationRule: ruleInteger destOrigin: 0@0 sourceOrigin: 0@0
		extent: patternBox extent clipRect: clipRectangle.
	bb colorMap:
		(patternForm colormapIfNeededFor: aDisplayMedium).
	(targetBox left truncateTo: patternBox width)
		to: targetBox right - 1 by: patternBox width do:
		[:x |
		(targetBox top truncateTo: patternBox height)
			to: targetBox bottom - 1 by: patternBox height do:
			[:y |
			bb destOrigin: x@y; copyBits]]
]

{ #category : #displaying }
InfiniteForm >> displayOnPort: aPort at: offset [

	| targetBox patternBox savedMap top left |

	self flag: #pharoFixMe. "this *may* not get called at the moment. I have been trying to figure out the right way for this to work and am using #displayOnPort:offsetBy: as my current offering - Bob"

	(patternForm isForm) ifFalse: [
		"patternForm is a Pattern or Color; just use it as a mask for BitBlt"
		^ aPort fill: aPort clipRect fillColor: patternForm rule: Form over].

	"do it iteratively"
	targetBox := aPort clipRect.
	patternBox := patternForm boundingBox.
	savedMap := aPort colorMap.
	aPort sourceForm: patternForm;
		fillColor: nil;
		combinationRule: Form paint;
		sourceRect: (0@0 extent: patternBox extent);
		colorMap: (patternForm colormapIfNeededFor: aPort destForm).
	top := (targetBox top truncateTo: patternBox height) "- (offset y \\ patternBox height)".
	left :=  (targetBox left truncateTo: patternBox width) "- (offset x \\ patternBox width)".
	left to: (targetBox right - 1) by: patternBox width do:
		[:x | top to: (targetBox bottom - 1) by: patternBox height do:
			[:y | aPort destOrigin: x@y; copyBits]].
	aPort colorMap: savedMap.

]

{ #category : #displaying }
InfiniteForm >> displayOnPort: aPort offsetBy: offset [

	| targetBox patternBox savedMap top left |

	"this version tries to get the form aligned where the user wants it and not just aligned with the cliprect"

	(patternForm isForm) ifFalse: [
		"patternForm is a Pattern or Color; just use it as a mask for BitBlt"
		^ aPort fill: aPort clipRect fillColor: patternForm rule: Form over].

	"do it iteratively"
	targetBox := aPort clipRect.
	patternBox := patternForm boundingBox.
	savedMap := aPort colorMap.
	aPort sourceForm: patternForm;
		fillColor: nil;
		combinationRule: Form paint;
		sourceRect: (0@0 extent: patternBox extent);
		colorMap: (patternForm colormapIfNeededFor: aPort destForm).
	top := (targetBox top truncateTo: patternBox height) + offset y.
	left :=  (targetBox left truncateTo: patternBox width) + offset x.

	left to: (targetBox right - 1) by: patternBox width do:
		[:x | top to: (targetBox bottom - 1) by: patternBox height do:
			[:y | aPort destOrigin: x@y; copyBits]].
	aPort colorMap: savedMap.

]

{ #category : #accessing }
InfiniteForm >> dominantColor [
	^ patternForm dominantColor
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> form [
	"Bitmap fills respond to #form"
	^patternForm
]

{ #category : #private }
InfiniteForm >> form: aForm [

	patternForm := aForm
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isBitmapFill [
	^true
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isGradientFill [
	^false
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isOrientedFill [
	^true
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isSolidFill [
	^false
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isTiled [
	"Return true if the receiver should be drawn as a tiled pattern"
	^true
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> isTranslucent [
	"Return true since the bitmap may be translucent and we don't really want to check"
	^true
]

{ #category : #testing }
InfiniteForm >> mightBeTranslucent [
	"Return true since the bitmap may be translucent and we don't really want to check"
	^true
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> normal [
	^0 @ patternForm height
]

{ #category : #accessing }
InfiniteForm >> offset [ 
	"Refer to the comment in DisplayObject|offset."

	^0 @ 0
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> origin [
	^0@0
]

{ #category : #'fillstyle protocol' }
InfiniteForm >> origin: aPoint [
	"Ignored"

]

{ #category : #displaying }
InfiniteForm >> raisedColor [
	^ Color transparent
]
