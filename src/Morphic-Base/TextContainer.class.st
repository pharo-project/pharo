"
A TextContainer models the shape of an ownerMorph, possibly occluded by one or more occludingMorphs, and scans this shape to provide a list of rectangles suitable for layout of text.  It does this by displaying the shadow of the ownerMorph in black, and any occludingMorphs in white, on its shadowForm.  It then scans horizontal strips of appropriate height to find unbroken intervals of black, greater than minWidth in extent.  Conputation of the rectangles is done on demand, and results are cached so that text can be redisplayed without having to recompute the rectangles.
"
Class {
	#name : #TextContainer,
	#superclass : #Object,
	#instVars : [
		'textMorph',
		'shadowForm',
		'vertProfile',
		'minWidth',
		'rectangleCache',
		'fillsOwner',
		'avoidsOcclusions'
	],
	#classVars : [
		'OuterMargin'
	],
	#category : #'Morphic-Base-Text Support'
}

{ #category : #initialization }
TextContainer class >> initialize [    "TextContainer initialize"
	OuterMargin := 2
]

{ #category : #access }
TextContainer >> avoidsOcclusions [
	^ avoidsOcclusions ifNil: [false]
]

{ #category : #access }
TextContainer >> avoidsOcclusions: aBoolean [
	avoidsOcclusions := aBoolean.
	self releaseCachedState
]

{ #category : #'container protocol' }
TextContainer >> bottom [
	"Note we should really check for contiguous pixels here"
	^ (self vertProfile findLast: [:count | count >= minWidth])
		+ shadowForm offset y
]

{ #category : #private }
TextContainer >> bounds [
	| bounds theText |
	self fillsOwner ifFalse: [^ textMorph textBounds].
	theText := textMorph.
	bounds := theText owner innerBounds.
	bounds := bounds insetBy: (textMorph valueOfProperty: #margins ifAbsent: [1@1]).
	theText owner submorphsBehind: theText do:
		[:m | bounds := bounds merge: m fullBounds].
	^ bounds
]

{ #category : #private }
TextContainer >> computeShadow [
	| canvas bounds theText |
	bounds := self bounds.
	theText := textMorph.
	canvas := (Display defaultCanvasClass extent: bounds extent depth: 1)
		asShadowDrawingCanvas: Color black.
	canvas translateBy: bounds topLeft negated during:[:tempCanvas| | back |
		self fillsOwner
			ifTrue: [tempCanvas fullDrawMorph: (theText owner copyWithoutSubmorph: theText)]
			ifFalse: [tempCanvas fillRectangle: textMorph bounds color: Color black].
		self avoidsOcclusions ifTrue:
			[back := tempCanvas form deepCopy.
			tempCanvas form fillWhite.
			theText owner submorphsInFrontOf: theText do:
				[:m | (textMorph isLinkedTo: m)
					ifTrue: []
					ifFalse: [tempCanvas fullDrawMorph: m]].
			back displayOn: tempCanvas form at: 0@0 rule: Form reverse].
	].
	shadowForm := canvas form offset: bounds topLeft.
	vertProfile := shadowForm  yTallyPixelValue: 1 orNot: false.
	rectangleCache := Dictionary new.
	^ shadowForm
]

{ #category : #access }
TextContainer >> fillsOwner [
	^ fillsOwner ifNil: [true]
]

{ #category : #access }
TextContainer >> fillsOwner: aBoolean [
	fillsOwner := aBoolean.
	self releaseCachedState
]

{ #category : #private }
TextContainer >> for: aTextMorph minWidth: wid [
	textMorph := aTextMorph.
	minWidth := wid.
	fillsOwner := true.
	avoidsOcclusions := false.
]

{ #category : #'container protocol' }
TextContainer >> left [ 
	^ textMorph owner left
]

{ #category : #'container protocol' }
TextContainer >> rectanglesAt: lineY height: lineHeight [ 
	"Return a list of rectangles that are at least minWidth wide
	in the specified horizontal strip of the shadowForm.
	Cache the results for later retrieval if the owner does not change."

	| hProfile rects thisWidth thisX count pair outerWidth lineRect lineForm |
	pair := Array with: lineY with: lineHeight.
	rects := rectangleCache at: pair ifAbsent: [nil].
	rects ifNotNil: [^rects].
	outerWidth := minWidth + (2 * OuterMargin).
	self shadowForm.	"Compute the shape"
	lineRect := 0 @ (lineY - shadowForm offset y) 
				extent: shadowForm width @ lineHeight.
	lineForm := shadowForm copy: lineRect.

	"Check for a full line -- frequent case"
	(lineForm tallyPixelValues second) = lineRect area 
		ifTrue: 
			[rects := Array with: (shadowForm offset x @ lineY extent: lineRect extent)]
		ifFalse: 
			["No such luck -- scan the horizontal profile for segments of minWidth"

			hProfile := lineForm xTallyPixelValue: 1 orNot: false.
			rects := OrderedCollection new.
			thisWidth := 0.
			thisX := 0.
			1 to: hProfile size
				do: 
					[:i | 
					count := hProfile at: i.
					count >= lineHeight 
						ifTrue: [thisWidth := thisWidth + 1]
						ifFalse: 
							[thisWidth >= outerWidth 
								ifTrue: 
									[rects addLast: ((thisX + shadowForm offset x) @ lineY 
												extent: thisWidth @ lineHeight)].
							thisWidth := 0.
							thisX := i]].
			thisWidth >= outerWidth 
				ifTrue: 
					[rects addLast: ((thisX + shadowForm offset x) @ lineY 
								extent: thisWidth @ lineHeight)]].
	rects := rects collect: [:r | r insetBy: OuterMargin @ 0].
	rectangleCache at: pair put: rects.
	^rects
]

{ #category : #access }
TextContainer >> releaseCachedState [

	shadowForm := nil.
	vertProfile := nil.
	rectangleCache := Dictionary new.

]

{ #category : #private }
TextContainer >> shadowForm [
	shadowForm ifNil: [self computeShadow].
	^ shadowForm
]

{ #category : #access }
TextContainer >> textMorph [
	^ textMorph
]

{ #category : #'container protocol' }
TextContainer >> top [
	"Note we should really check for contiguous pixels here"
	| outerWidth |
	outerWidth := minWidth + (2*OuterMargin).
	^ (self vertProfile findFirst: [:count | count >= outerWidth]) - 1
		+ shadowForm offset y
]

{ #category : #'container protocol' }
TextContainer >> topLeft [  "for compatibility"
	^ textMorph owner topLeft
]

{ #category : #'container protocol' }
TextContainer >> translateBy: delta [
	self releaseCachedState
]

{ #category : #private }
TextContainer >> vertProfile [
	vertProfile ifNil: [self computeShadow].
	^ vertProfile
]

{ #category : #'container protocol' }
TextContainer >> width [  "for compatibility"
	^ textMorph owner width
]
