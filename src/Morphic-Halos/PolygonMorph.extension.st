Extension { #name : 'PolygonMorph' }

{ #category : '*Morphic-Halos' }
PolygonMorph >> addCustomMenuItems: aMenu hand: aHandMorph [
	| |
	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu
		addUpdating: #handlesShowingPhrase
		target: self
		selector: #showOrHideHandles.
	vertices size > 2
		ifTrue: [ self addPolyLIneCurveMenuItems: aMenu hand: aHandMorph ].
	aMenu add: 'specify dashed line' selector: #specifyDashedLine.
	self isOpen
		ifTrue: [self addPolyArrowMenuItems: aMenu hand: aHandMorph]
			ifFalse: [self addPolyShapingMenuItems: aMenu hand: aHandMorph]
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> addPolyArrowMenuItems: aMenu hand: aHandMorph [
	aMenu addLine.
	aMenu addWithLabel: '---' enablement: [ self isOpen and: [ arrows ~~ #none ] ] action: #makeNoArrows.
	aMenu addWithLabel: '-->' enablement: [ self isOpen and: [ arrows ~~ #forward ] ] action: #makeForwardArrow.
	aMenu addWithLabel: '<--' enablement: [ self isOpen and: [ arrows ~~ #back ] ] action: #makeBackArrow.
	aMenu addWithLabel: '<->' enablement: [ self isOpen and: [ arrows ~~ #both ] ] action: #makeBothArrows.
	aMenu add: 'customize arrows' selector: #customizeArrows:.
	(self hasProperty: #arrowSpec)
		ifTrue: [ aMenu add: 'standard arrows' selector: #standardArrows ]
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> addPolyLIneCurveMenuItems: aMenu hand: aHandMorph [

	aMenu addLine;
				addUpdating: #openOrClosePhrase
				target: self
				selector: #toggleOpenOrClosed.

			aMenu
				addUpdating: #smoothOrSegmentedPhrase
				target: self
				selector: #toggleSmoothing
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> addPolyShapingMenuItems: aMenu hand: aHandMorph [
	aMenu addLine.
	aMenu addWithLabel: 'make inscribed diamondOval' enablement: [ self isClosed ] action: #diamondOval.
	aMenu addWithLabel: 'make enclosing rectangleOval' enablement: [ self isClosed ] action: #rectangleOval
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> arrowLength: aLength [
	"Assumes that I have exactly two vertices"

	| theta horizontalOffset verticalOffset newTip delta |
	delta := vertices second - vertices first.
	theta := delta theta.
	horizontalOffset := aLength * (theta cos).
	verticalOffset := aLength * (theta sin).
	newTip := vertices first + (horizontalOffset @ verticalOffset).
	self verticesAt: 2 put: newTip
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> arrowSpec: specPt [
	"Specify a custom arrow for this line.
	specPt x abs gives the length of the arrow (point to base) in terms of borderWidth.
	If specPt x is negative, then the base of the arrow will be concave.
	specPt y abs gives the width of the arrow.
	The standard arrow is equivalent to arrowSpec: PolygonMorph defaultArrowSpec.
	See arrowBoundsAt:From: for details."

	self setProperty: #arrowSpec toValue: specPt.
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> arrows [
	^arrows
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> customizeArrows: evt [
	| handle origin aHand |
	aHand := evt ifNil: [self primaryHand] ifNotNil: [evt hand].
	origin := aHand position.
	handle := HandleMorph new
		forEachPointDo:
			[:newPoint | handle removeAllMorphs.
			handle addMorph:
				(LineMorph from: origin to: newPoint color: Color black width: 1).
			self arrowSpec: (newPoint - origin) / 5.0]
		lastPointDo:
			[:newPoint | handle deleteBalloon.
			self halo ifNotNil: [:halo | halo addHandles].].
	aHand attachMorph: handle.
	handle showBalloon:
'Move cursor left and right
to change arrow length and style.
Move it up and down to change width.
Click when done.' hand: evt hand.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> handlesShowingPhrase [
	^ (self showingHandles
		ifTrue: ['hide handles']
		ifFalse: ['show handles']) translated
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> makeBackArrow [
	arrows := #back.
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> makeBothArrows [
	arrows := #both.
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> makeNoArrows [
	arrows := #none.
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> showOrHideHandles [
	self showingHandles
		ifTrue:	[self removeHandles]
		ifFalse:	[self addHandles]
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> showingHandles [

	^ handles isNotNil
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> specifyDashedLine [

	| executableSpec newSpec |
	executableSpec := self morphicUIManager
		request:
'Enter a dash specification as
{ major dash length. minor dash length. minor dash color }
The major dash will have the normal border color.
A blank response will remove the dash specification.
[Note: You may give 5 items as, eg, {10. 5. Color white. 0. 3}
where the 4th ityem is zero, and the 5th is the number of pixels
by which the dashes will move in each step of animation]' translated
		initialAnswer: '{ 10. 5. Color red }'.
	executableSpec isEmptyOrNil ifTrue:
		[^ self stopStepping; dashedBorder: nil].
	newSpec := [self class compiler evaluate: executableSpec] onErrorDo:
		[^ self stopStepping; dashedBorder: nil].
	(newSpec first isNumber and: [newSpec second isNumber and: [newSpec third isColor]]) ifFalse:
		[^ self stopStepping; dashedBorder: nil].
	newSpec size = 3 ifTrue:
		[^ self stopStepping; dashedBorder: newSpec].
	(newSpec size = 5 and: [newSpec fourth isNumber and: [newSpec fifth isNumber]]) ifTrue:
		[^ self dashedBorder: newSpec; startStepping]
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> standardArrows [

	self removeProperty: #arrowSpec.
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> toggleSmoothing [

	smoothCurve := smoothCurve not.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> unrotatedLength [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue:
		[^ (vertices second - vertices first) r].

	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> unrotatedLength: aLength [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue: [^ self arrowLength: aLength].

	self setVertices: ((((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height: aLength) rotationDegrees: 0) vertices
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> unrotatedWidth [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue: [^ self borderWidth].
	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) width
]

{ #category : '*Morphic-Halos' }
PolygonMorph >> unrotatedWidth: aWidth [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	self borderWidth: aWidth
]
