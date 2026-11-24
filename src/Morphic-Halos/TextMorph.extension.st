Extension { #name : 'TextMorph' }

{ #category : '*Morphic-Halos' }
TextMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	| outer |

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addUpdating: #autoFitString target: self selector: #autoFitOnOff.
	aCustomMenu addUpdating: #wrapString target: self selector: #wrapOnOff.
	aCustomMenu add: 'text margins...' selector: #changeMargins:.
	aCustomMenu add: 'add predecessor' selector: #addPredecessor:.
	aCustomMenu add: 'add successor' selector: #addSuccessor:.
	aCustomMenu add: 'code pane menu...' selector: #yellowButtonActivity.

	outer := self owner.
	outer
		ifNotNil: [ outer isLineMorph
				ifTrue: [ container ifNotNil: [ aCustomMenu add: 'set baseline' selector: #setCurveBaseline: ] ]
				ifFalse: [ self fillsOwner
						ifFalse: [ aCustomMenu add: 'fill owner''s shape' selector: #fillingOnOff ]
						ifTrue: [ aCustomMenu add: 'rectangular bounds' selector: #fillingOnOff ].
					self avoidsOcclusions
						ifFalse: [ aCustomMenu add: 'avoid occlusions' selector: #occlusionsOnOff ]
						ifTrue: [ aCustomMenu add: 'ignore occlusions' selector: #occlusionsOnOff ]
					]
			]
]

{ #category : '*Morphic-Halos' }
TextMorph >> autoFitString [
	"Answer the string to put in a menu that will invite the user to
	switch autoFit mode"
	^ (self isAutoFit
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text auto fit' translated
]

{ #category : '*Morphic-Halos' }
TextMorph >> changeMargins: evt [
	| handle origin aHand newMargin |
	aHand := evt ifNil: [self primaryHand] ifNotNil: [evt hand].
	origin := aHand position.
	handle := HandleMorph new
		forEachPointDo:
			[:newPoint | handle removeAllMorphs.
			handle addMorph:
				(LineMorph from: origin to: newPoint color: Color black width: 1).
			newMargin := (newPoint - origin max: 0@0) // 5.
			self margins: newMargin asMargin]
		lastPointDo:
			[:newPoint | handle deleteBalloon.
			self halo ifNotNil: [:halo | halo addHandles]].
	aHand attachMorph: handle.
	handle showBalloon:
'Move cursor down and to the right
to increase margin inset.
Click when done.' hand: evt hand.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
TextMorph >> setCurveBaseline: evt [
	| handle origin |
	origin := evt cursorPoint.
	handle := HandleMorph new forEachPointDo:
		[:newPoint | handle removeAllMorphs.
		handle addMorph:
			(PolygonMorph vertices: (Array with: origin with: newPoint)
				color: Color black borderWidth: 1 borderColor: Color black).
		container baseline: (newPoint - origin) y negated asInteger // 5.
		self paragraph composeAll].
	evt hand attachMorph: handle.
	handle startStepping
]

{ #category : '*Morphic-Halos' }
TextMorph >> wrapOnOff [
	self wrapFlag: wrapFlag not
]

{ #category : '*Morphic-Halos' }
TextMorph >> wrapString [
	"Answer the string to put in a menu that will invite the user to
	switch autoFit mode"
	^ (wrapFlag
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text wrap to bounds' translated
]
