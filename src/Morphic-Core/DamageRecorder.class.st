"
Recording damages to repair by repainting
"
Class {
	#name : #DamageRecorder,
	#superclass : #Object,
	#instVars : [
		'invalidRects',
		'totalRepaint'
	],
	#category : #'Morphic-Core-Support'
}

{ #category : #'instance creation' }
DamageRecorder class >> new [

	^ super new reset

]

{ #category : #recording }
DamageRecorder >> doFullRepaint [
	"Record that a full redisplay is needed. No further damage rectangles will be recorded until after the next reset."

	^ totalRepaint := true.

]

{ #category : #recording }
DamageRecorder >> invalidRectsFullBounds: aRectangle [
	"Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle."

	^ totalRepaint
		ifTrue: [ Array with: aRectangle]
		ifFalse: [ invalidRects copy].


]

{ #category : #recording }
DamageRecorder >> recordInvalidRect: newRect [
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle."

	| mergeRect zeroRect |
	totalRepaint ifTrue: [^ self].  "planning full repaint; don't bother collecting damage"
	zeroRect := 0@0 corner: 0@0. 
	invalidRects do:
		[:rect | | a |
		((a := (rect intersect: newRect ifNone: [ zeroRect ]) area) > 40
			and: ["Avoid combining a vertical and horizontal rects.
				  Can make a big diff and we only test when likely."
				  a > (newRect area // 4) or: [a > (rect area // 4)]])
			ifTrue:
			["merge rectangle in place (see note below) if there is significant overlap"
			rect setPoint: (rect origin min: newRect origin) truncated
				point: (rect corner max: newRect corner) truncated.
			^ self]].


	invalidRects size >= 50 ifTrue:
		["if there are too many separate areas, merge them all"
		mergeRect := Rectangle merging: invalidRects.
		self reset.
		invalidRects addLast: mergeRect].

	"add the given rectangle to the damage list"
	"Note: We make a deep copy of all rectangles added to the damage list,
		since rectangles in this list may be extended in place."
	newRect hasPositiveExtent ifTrue: [ 
		invalidRects addLast:
			(newRect topLeft truncated corner: newRect bottomRight truncated).
	].
]

{ #category : #initialization }
DamageRecorder >> reset [
	"Clear the damage list."

	invalidRects := OrderedCollection new: 15.
	totalRepaint := false

]

{ #category : #testing }
DamageRecorder >> updateIsNeeded [
	"Return true if the display needs to be updated."

	^totalRepaint or: [invalidRects notEmpty]
]
