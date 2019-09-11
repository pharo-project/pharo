Extension { #name : #BorderedMorph }

{ #category : #'*Morphic-Base' }
BorderedMorph >> changeBorderColor: evt [
	self theme
		chooseColorIn: self
		title: 'Choose Color' translated
		color: self borderStyle color
		for: [ :newColor | self borderColor: newColor ]
]

{ #category : #'*Morphic-Base' }
BorderedMorph >> changeBorderWidth: evt [
	| aHand origin handle newWidth |
	aHand := evt ifNil: [self primaryHand] ifNotNil: [evt hand].
	origin := aHand position.
	handle := HandleMorph new
		forEachPointDo:
			[:newPoint | handle removeAllMorphs.
			handle addMorph:
				(LineMorph from: origin to: newPoint color: Color black width: 1).
			newWidth := (newPoint - origin) r asInteger // 5.
			self borderWidth: newWidth]
		lastPointDo:
			[:newPoint | handle deleteBalloon.
			self halo ifNotNil: [:halo | halo addHandles]].
	aHand attachMorph: handle.
	handle showBalloon:
'Move cursor farther from
this point to increase border width.
Click when done.' translated hand: evt hand.
	handle startStepping
]

{ #category : #'*Morphic-Base' }
BorderedMorph class >> exampleGradient [
	"self exampleGradient"
	
	| morph fs |
	morph := BorderedMorph new.
	fs := GradientFillStyle ramp: {0.0 -> Color red. 1.0 -> Color green}.
	fs origin: morph bounds center.
	fs direction: (morph bounds width // 2) @ 0.
	fs radial: true.
	morph fillStyle: fs.
	self currentWorld primaryHand attachMorph: morph.
]
