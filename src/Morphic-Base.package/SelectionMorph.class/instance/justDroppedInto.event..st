dropping/grabbing
justDroppedInto: newOwner event: evt

	selectedItems isEmpty ifTrue:
		["Hand just clicked down to draw out a new selection"
		^ self extendByHand: evt hand].
	dupLoc ifNotNil: [dupDelta := self position - dupLoc].
	selectedItems reverseDo: [:m | 
		self defer:
			[m referencePosition: (newOwner localPointToGlobal: m referencePosition).
			newOwner handleDropMorph:
				(DropEvent new setPosition: evt cursorPoint contents: m hand: evt hand)]].
	evt wasHandled: true