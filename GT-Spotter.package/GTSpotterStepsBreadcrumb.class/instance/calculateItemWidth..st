geometry
calculateItemWidth: aBrick
	|sWidth bWidth singleWidth lastWidth|
	sWidth := self themer spotterThemer breadcrumbSeparatorWidth.
	bWidth := self width.
	lastWidth := ((aBrick labelBrick approximateWidth min: (bWidth / 2)) max: ((bWidth - ((self maxNumberOfSteps - 1) * sWidth)) / self maxNumberOfSteps)) floor.
	singleWidth := ((bWidth - lastWidth - ((self maxNumberOfSteps - 1) * sWidth)) / (self maxNumberOfSteps - 1)) floor.
	
	"width of other bricks depends on the width of the last one"
	^ aBrick isLastInOwner
		ifTrue: [
			self items size >= self maxNumberOfSteps ifTrue: [ |fullWidth delta|
				fullWidth := lastWidth + ((self maxNumberOfSteps - 1) * (sWidth + singleWidth)).
				delta := bWidth - fullWidth.
				lastWidth := lastWidth + delta ].
			lastWidth ]
		ifFalse: [ singleWidth ]
	