event handling
mouseMove: evt

	self inState: #panning do: [ :startPanAndPos |
		| delta |
		delta := startPanAndPos first + ( evt cursorPoint - startPanAndPos second ).
		
		transform 
			x: delta x;
			y: delta y.
		self changed.
	] 