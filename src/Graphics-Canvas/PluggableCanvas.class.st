"
An abstract canvas which modifies the behavior of an underlying canvas in some way.  Subclasses should implement apply:, which takes a one argument block and an actual canvas to draw on.  See apply: for the specific definition.
"
Class {
	#name : #PluggableCanvas,
	#superclass : #Canvas,
	#category : #'Graphics-Canvas'
}

{ #category : #private }
PluggableCanvas >> apply: aBlock [
	"evaluate aBlock with a canvas to do a drawing command on.  See implementors for examples"
]

{ #category : #'canvas methods' }
PluggableCanvas >> balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [

	self apply: [ :c | 
		c balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc
	]
]

{ #category : #'canvas methods' }
PluggableCanvas >> balloonFillRectangle: aRectangle fillStyle: aFillStyle [

	self apply: [ :c | c balloonFillRectangle: aRectangle fillStyle: aFillStyle ]
]

{ #category : #'drawing-support' }
PluggableCanvas >> clipBy: newClipRect during: aBlock [
	self apply: [ :c |
		c clipBy: newClipRect during: aBlock ]
]

{ #category : #accessing }
PluggableCanvas >> clipRect [
	| innerClipRect |
	self apply: [ :c |
		innerClipRect := c clipRect ].
	^innerClipRect
]

{ #category : #accessing }
PluggableCanvas >> contentsOfArea: aRectangle into: aForm [
	self apply: [ :c |
		c contentsOfArea: aRectangle into: aForm ].
	^aForm
]

{ #category : #'drawing-polygons' }
PluggableCanvas >> drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc [
	self apply: [ :c |
		c drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc ]
]

{ #category : #'drawing-text' }
PluggableCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [
	self apply: [ :clippedCanvas |
		clippedCanvas drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c]
]

{ #category : #'drawing-text' }
PluggableCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [
	self apply: [ :clippedCanvas |
		clippedCanvas drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc]
]

{ #category : #accessing }
PluggableCanvas >> extent [

	self apply: [ :c | ^c extent ].

]

{ #category : #'drawing-ovals' }
PluggableCanvas >> fillOval: r color: c borderWidth: borderWidth borderColor: borderColor [
	self apply: [ :clippedCanvas |
		clippedCanvas fillOval: r color: c borderWidth: borderWidth borderColor: borderColor ]
]

{ #category : #'drawing-ovals' }
PluggableCanvas >> fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc [
	"Fill the given oval."
	(aFillStyle isBitmapFill and:[aFillStyle isKindOf: InfiniteForm]) ifTrue:[
		self flag: #pharoFixMe. "See FormCanvas>>fillOval:fillStyle:borderWidth:borderColor:"
		^self fillOval: aRectangle color: aFillStyle borderWidth: bw borderColor: bc].
	(aFillStyle isSolidFill) ifTrue:[
		^self fillOval: aRectangle color: aFillStyle asColor borderWidth: bw borderColor: bc].
	"Use a BalloonCanvas instead"
	self balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc
]

{ #category : #'drawing-rectangles' }
PluggableCanvas >> fillRectangle: aRectangle basicFillStyle: aFillStyle [
	"Fill the given rectangle with the given, non-composite, fill style."

	| pattern |

	(aFillStyle isKindOf: InfiniteForm) ifTrue: [
		^self infiniteFillRectangle: aRectangle fillStyle: aFillStyle
	].

	aFillStyle isSolidFill ifTrue:[ ^self fillRectangle: aRectangle color: aFillStyle asColor].

	"We have a very special case for filling with infinite forms"
	(aFillStyle isBitmapFill and:[aFillStyle origin = (0@0)]) ifTrue:[
		pattern := aFillStyle form.
		(aFillStyle direction = (pattern width @ 0) 
			and:[aFillStyle normal = (0@pattern height)]) ifTrue:[
				"Can use an InfiniteForm"
				^self fillRectangle: aRectangle color: (InfiniteForm with: pattern)].
	].
	"Use a BalloonCanvas instead"
	self balloonFillRectangle: aRectangle fillStyle: aFillStyle.

]

{ #category : #initialization }
PluggableCanvas >> flush [
	self apply: [ :c |
		c flush ]
]

{ #category : #other }
PluggableCanvas >> flushDisplay [
	self apply: [ :c |
		c flushDisplay ]
]

{ #category : #other }
PluggableCanvas >> forceToScreen: rect [

	self apply: [ :c |
		c forceToScreen: rect ]
]

{ #category : #'drawing-rectangles' }
PluggableCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [
	self apply: [ :c |
		c frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor ]
]

{ #category : #private }
PluggableCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [
	self apply:  [ :c |
		c image: aForm at: aPoint sourceRect: sourceRect rule: rule ]
]

{ #category : #'canvas methods' }
PluggableCanvas >> infiniteFillRectangle: aRectangle fillStyle: aFillStyle [

	self apply: [ :c | c infiniteFillRectangle: aRectangle fillStyle: aFillStyle ]
]

{ #category : #drawing }
PluggableCanvas >> line: pt1 to: pt2 width: w color: c [
	self apply: [ :clippedCanvas |
		clippedCanvas line: pt1 to: pt2 width: w color: c ]
]

{ #category : #accessing }
PluggableCanvas >> origin [

	self apply: [ :c | ^c origin ].

]

{ #category : #'drawing-images' }
PluggableCanvas >> paintImage: aForm at: aPoint [
	self apply: [ :c |
		c paintImage: aForm at: aPoint ]
]

{ #category : #'drawing-images' }
PluggableCanvas >> paintImage: aForm at: aPoint sourceRect: sourceRect [
	self apply: [ :c |
		c paintImage: aForm at: aPoint sourceRect: sourceRect ]
]

{ #category : #drawing }
PluggableCanvas >> paragraph: paragraph bounds: bounds color: color [
	self apply: [ :c |
		c paragraph: paragraph bounds: bounds color: color ]
]

{ #category : #drawing }
PluggableCanvas >> render: anObject [
	self apply: [ :c |
		c render: anObject ]
]

{ #category : #'drawing-general' }
PluggableCanvas >> roundCornersOf: aMorph in: bounds during: aBlock [
	aMorph wantsRoundedCorners ifFalse:[^aBlock value].
	(self seesNothingOutside: (CornerRounder rectWithinCornersOf: bounds))
		ifTrue: ["Don't bother with corner logic if the region is inside them"
				^ aBlock value].
	CornerRounder roundCornersOf: aMorph on: self in: bounds
		displayBlock: aBlock
		borderWidth: aMorph borderWidthForRounding
		corners: aMorph roundedCorners
]

{ #category : #accessing }
PluggableCanvas >> shadowColor: color [
	self apply: [ :c |
		c shadowColor: color ]
]

{ #category : #'canvas methods' }
PluggableCanvas >> showAt: pt invalidRects: updateRects [
	self apply: [ :c |
		c showAt: pt invalidRects: updateRects ]
]

{ #category : #'drawing-images' }
PluggableCanvas >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [
	self apply: [ :c |
		c stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor ]
]

{ #category : #'drawing-support' }
PluggableCanvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [

	self apply: [ :clippedCanvas |
		clippedCanvas transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize ]
]

{ #category : #other }
PluggableCanvas >> translateBy: aPoint clippingTo: aRect during: aBlock [
	self apply: [ :clippedCanvas |
		clippedCanvas translateBy: aPoint clippingTo: aRect during: aBlock ]
]

{ #category : #'drawing-support' }
PluggableCanvas >> translateBy: delta during: aBlock [
	self apply: [ :clippedCanvas |
		 clippedCanvas translateBy: delta during: aBlock ]
]
