"
This class is a quick hack to support rounded corners in morphic.

Rather than produce rounded rectangles, it tweaks the display of corners.
Rather than work for any radius, it only supports a radius of 6.
Rather than work for any border width, it only supports widths 0, 1 and 2.
The corners, while apparently transparent, still behave opaquely to mouse clicks.

Worse than this, the approach relies on the ability to extract underlying bits from the canvas prior to display.  This ran afoul of top-down display, it seems, in SystemWindow spawnReframeHandle: (qv).  It will also make a postscript printer very unhappy.

But, hey, it's cute.
"
Class {
	#name : #CornerRounder,
	#superclass : #Object,
	#instVars : [
		'cornerMasks',
		'cornerOverlays',
		'underBits'
	],
	#classVars : [
		'CR0',
		'CR1',
		'CR2'
	],
	#category : #'Graphics-Canvas'
}

{ #category : #all }
CornerRounder class >> initialize [  "CornerRounder initialize"

	CR0 := CR1 := self new
		masterMask:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11111e26 2r11111e26 2r111111e26)
				offset: 0@0)
		masterOverlay:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r110e26 2r1000e26 2r10000e26 2r10000e26 2r100000e26)
				offset: 0@0).
	CR2 := self new
		masterMask:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11111e26 2r11111e26 2r111111e26)
				offset: 0@0)
		masterOverlay:
			(Form extent: 6@6
				fromArray: #(2r1e26 2r111e26 2r1111e26 2r11100e26 2r11000e26 2r111000e26)
				offset: 0@0).


]

{ #category : #all }
CornerRounder class >> rectWithinCornersOf: aRectangle [
	"Return a single sub-rectangle that lies entirely inside corners
	that are made by me.
	Used to identify large regions of window that do not need to be redrawn."

	^ aRectangle insetBy: 0@6
]

{ #category : #all }
CornerRounder class >> roundCornersOf: aMorph on: aCanvas in: bounds displayBlock: displayBlock borderWidth: w corners: aList [

	| rounder |
	rounder := CR0.
	w = 1 ifTrue: [rounder := CR1].
	w = 2 ifTrue: [rounder := CR2].
	rounder := rounder copy.
	rounder saveBitsUnderCornersOf: aMorph on: aCanvas in: bounds corners: aList.
	displayBlock value.
	rounder tweakCornersOf: aMorph on: aCanvas in: bounds borderWidth: w corners: aList
]

{ #category : #all }
CornerRounder class >> roundShadowCornersOf: aMorph on: aCanvas in: bounds displayBlock: displayBlock borderWidth: w corners: aList [

	| rounder |
	rounder := CR0.
	w = 1 ifTrue: [rounder := CR1].
	w = 2 ifTrue: [rounder := CR2].
	rounder := rounder copy.
	rounder saveBitsUnderCornersOf: aMorph on: aCanvas in: bounds corners: aList.
	displayBlock value.
	rounder tweakShadowCornersOf: aMorph on: aCanvas in: bounds borderWidth: w corners: aList
]

{ #category : #all }
CornerRounder >> masterMask: maskForm masterOverlay: overlayForm [

	cornerMasks := #(none left pi right) collect:
		[:dir | (maskForm rotateBy: dir centerAt: 0@0) offset: 0@0].
	cornerOverlays := #(none left pi right) collect:
		[:dir | (overlayForm rotateBy: dir centerAt: 0@0) offset: 0@0].

]

{ #category : #all }
CornerRounder >> saveBitsUnderCornersOf: aMorph on: aCanvas in: bounds corners: cornerList [

	| corners |
	underBits := Array new: 4.
	corners := bounds corners.
	cornerList do:[:i|
		| offset corner mask form rect |
		mask := cornerMasks at: i.
		corner := corners at: i.
		i = 1 ifTrue: [offset := 0@0].
		i = 2 ifTrue: [offset := 0@mask height negated].
		i = 3 ifTrue: [offset := mask extent negated].
		i = 4 ifTrue: [offset := mask width negated@0].
		rect := corner + offset extent: mask extent.
		(aCanvas isVisible: rect) ifTrue:[
			form := aCanvas contentsOfArea: rect.
			form copyBits: form boundingBox from: mask at: 0@0 clippingBox: form boundingBox rule: Form and fillColor: nil map: (Bitmap with: 16rFFFFFFFF with: 0).
			underBits at: i put: form]].

]

{ #category : #all }
CornerRounder >> tweakCornersOf: aMorph on: aCanvas in: bounds borderWidth: w corners: cornerList [
	"This variant has a cornerList argument, to allow some corners to be rounded and others not"
	| fourColors mask corners |

	w > 0 ifTrue:[ fourColors := aMorph borderStyle colorsAtCorners ].
	mask := Form extent: cornerMasks first extent depth: aCanvas depth.
	corners := bounds corners.
	cornerList do:[:i|
		| offset corner saveBits outBits |
		corner := corners at: i.
		saveBits := underBits at: i.
		saveBits ifNotNil:[
			i = 1 ifTrue: [offset := 0@0].
			i = 2 ifTrue: [offset := 0@saveBits height negated].
			i = 3 ifTrue: [offset := saveBits extent negated].
			i = 4 ifTrue: [offset := saveBits width negated@0].

			"Mask out corner area (painting saveBits won't clear if transparent)."
			mask copyBits: mask boundingBox from: (cornerMasks at: i) at: 0@0 clippingBox: mask boundingBox rule: Form over fillColor: nil map: (Bitmap with: 0 with: 16rFFFFFFFF).
			outBits := aCanvas contentsOfArea: (corner + offset extent: mask extent).
			mask displayOn: outBits at: 0@0 rule: Form and.
			"Paint back corner bits."
			saveBits displayOn: outBits at: 0@0 rule: Form paint.
			"Paint back corner bits."
			aCanvas drawImage: outBits at: corner + offset.

			w > 0 ifTrue:[
				
				aCanvas stencil: (cornerOverlays at: i) at: corner + offset
						color: (fourColors at: i)]]].

]

{ #category : #all }
CornerRounder >> tweakShadowCornersOf: aMorph on: aCanvas in: bounds borderWidth: w corners: cornerList [
	"This variant has a cornerList argument, to allow some corners to be rounded and others not"
	| fourColors mask corners |

	w > 0 ifTrue: [fourColors := Array new: 4 withAll: Color transparent].
	mask := Form extent: cornerMasks first extent depth: aCanvas depth.
	corners := bounds corners.
	cornerList do:[:i|
		| offset corner saveBits outBits |
		corner := corners at: i.
		saveBits := underBits at: i.
		saveBits ifNotNil:[
			i = 1 ifTrue: [offset := 0@0].
			i = 2 ifTrue: [offset := 0@saveBits height negated].
			i = 3 ifTrue: [offset := saveBits extent negated].
			i = 4 ifTrue: [offset := saveBits width negated@0].

			"Mask out corner area (painting saveBits won't clear if transparent)."
			mask copyBits: mask boundingBox from: (cornerMasks at: i) at: 0@0 clippingBox: mask boundingBox rule: Form over fillColor: nil map: (Bitmap with: 0 with: 16rFFFFFFFF).
			outBits := aCanvas contentsOfArea: (corner + offset extent: mask extent).
			mask displayOn: outBits at: 0@0 rule: Form and.
			"Paint back corner bits."
			saveBits displayOn: outBits at: 0@0 rule: Form paint.
			"Paint back corner bits."
			aCanvas drawImage: outBits at: corner + offset.

			w > 0 ifTrue:[
				
				aCanvas stencil: (cornerOverlays at: i) at: corner + offset
						color: (fourColors at: i)]]].

]
