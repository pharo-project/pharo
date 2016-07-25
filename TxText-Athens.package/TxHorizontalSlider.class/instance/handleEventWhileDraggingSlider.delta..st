as yet unclassified 
handleEventWhileDraggingSlider: event delta: delta

	| deltaPos newPosition ann |
	event redButtonPressed ifFalse: [ ^ false ]. "finished"
	
	event isMove ifTrue: [ 
		"adjust slider position relative to dragged position"

		deltaPos :=  (event position x - bounds left) - delta.
		newPosition := deltaPos / bounds width.
		ann := (TxSliderDragged source: self) oldPosition: sliderPosition; newPosition: newPosition; yourself.
		self sliderPosition: newPosition.
		
		self announcer announce: ann.
	 ].
	

	^ true.