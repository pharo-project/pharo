displaying
displayString: string from: startIndex to: stopIndex at: aPoint
	canvas pathTransform
		restoreAfter: [ 
			canvas pathTransform translateBy: aPoint + (0@font ascent).
			canvas drawString: string from: startIndex to: stopIndex ]