rendering
renderHighlightOn: canvas
	
	canvas pathTransform restoreAfter: [
		canvas pathTransform translateBy: layoutView offset negated.
		canvas setPaint: paint.
		self shapes do: [ :e | canvas drawShape: e ]
	].