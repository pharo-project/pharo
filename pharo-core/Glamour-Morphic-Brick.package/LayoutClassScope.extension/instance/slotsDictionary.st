*Glamour-Morphic-Brick
slotsDictionary
	| result |
	result := Dictionary new.
	self allSlotsDo: [ :slot |  result at: slot name put: slot].
	^ result