private
privateFullMoveBy: delta
	| deltaSlipped griddingMorph |
	selectedItems isEmpty
		ifTrue: [ ^ super privateFullMoveBy: delta ].
	griddingMorph := self pasteUpMorph.
	griddingMorph ifNil: [ ^ super privateFullMoveBy: delta ].
	deltaSlipped := delta + slippage.
	slippage := 0.
	super privateFullMoveBy: deltaSlipped.
	selectedItems do: [ :m | m position: m position + deltaSlipped ]