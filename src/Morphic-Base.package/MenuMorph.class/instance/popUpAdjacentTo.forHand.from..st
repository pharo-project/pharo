events
popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem 
	"Present this menu at the given point under control of the given  
	hand."
	| tryToPlace selectedOffset |
	hand world startSteppingSubmorphsOf: self.
	popUpOwner := sourceItem.
	self fullBounds.
self updateColor.
	"ensure layout is current"
	selectedOffset := selectedItem ifNil: [
		self items 
			ifEmpty: [ 0@0 ]
			ifNotEmpty: [ :col | col first position - self position. ]
	] ifNotNil: [ selectedItem position - self position ].
					
	tryToPlace := [:where :mustFit | | delta | 
			self position: where - selectedOffset.
			delta := self fullBoundsInWorld amountToTranslateWithin: sourceItem worldBounds.
			(delta x = 0
					or: [mustFit])
				ifTrue: [delta = (0 @ 0)
						ifFalse: [self position: self position + delta].
					sourceItem world addMorphFront: self.
					^ self]].
	tryToPlace value: rightOrLeftPoint first value: false;
		 value: rightOrLeftPoint last - (self width @ 0) value: false;
		 value: rightOrLeftPoint first value: true