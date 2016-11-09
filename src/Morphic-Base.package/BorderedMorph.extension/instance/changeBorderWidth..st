*Morphic-Base
changeBorderWidth: evt
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