Extension { #name : 'BorderedMorph' }

{ #category : '*Morphic-Halos' }
BorderedMorph >> addBorderStyleMenuItems: aMenu hand: aHandMorph [
	"Add border-style menu items"

	| subMenu |
	subMenu := self morphicUIManager newMenuIn: self for: self.
	subMenu
		add: 'border width...' selector: #changeBorderWidth:;
		add: 'border color...' selector: #changeBorderColor:;
		addLine.

	BorderStyle borderStyleChoices do:
		[:sym | (self borderStyleForSymbol: sym)
			ifNotNil:
				[subMenu add: sym target: self selector: #setBorderStyle: argument: sym]].
	aMenu add: 'border style' subMenu: subMenu
]

{ #category : '*Morphic-Halos' }
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
