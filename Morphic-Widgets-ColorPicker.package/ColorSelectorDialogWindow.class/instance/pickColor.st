actions
pickColor
	"Pick a colour from the screen."

	| p c h |
	h := self activeHand.
	h
		showTemporaryCursor: (Smalltalk ui icons iconNamed: #eyedropperIcon)
		hotSpotOffset: 6 @ 31.
	h
		captureEventsUntil: [ :evt | 
			evt isMove
				ifTrue: [ p := evt position.
					(self hsvaMorph containsPoint: p)
						ifFalse:
							[ "deal with the fact that 32 bit displays may have garbage in the alpha bits"
							c := Display depth = 32
								ifTrue: [ Color
										colorFromPixelValue: ((Display pixelValueAt: p) bitOr: 16rFF000000)
										depth: 32 ]
								ifFalse: [ Display colorAt: p ] ].
					h position: p.
					self selectedColor ~= c
						ifTrue: [ self selectedColor: c ] ].
			"			self world displayWorldSafely. "
			h anyButtonPressed ].
	h showTemporaryCursor: nil