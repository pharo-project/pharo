private
positionAt: aPoint relativeTo: aMenuItem inWorld: aWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |	
	self fullBounds. "force layout"
	i := 0.
	yOffset := 0.
	[(sub := self submorphs at: (i := i + 1)) == aMenuItem]
		whileFalse: [yOffset := yOffset + sub height].

	self position: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self right > aWorld worldBounds right
		ifTrue:
			[self right: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta := self bounds amountToTranslateWithin:
		(aWorld worldBounds withHeight: ((aWorld worldBounds height - 18) max: (ActiveHand position y) + 1)).
	delta = (0 @ 0) ifFalse: [self position: self position + delta]