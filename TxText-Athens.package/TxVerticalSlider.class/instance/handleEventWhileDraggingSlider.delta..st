event handling
handleEventWhileDraggingSlider: event delta: delta

	| deltaPos newPosition ann |
	event redButtonPressed ifFalse: [ ^ false ]. "finished"
	
	event isMove ifTrue: [ 
		"adjust slider position relative to dragged position"

		deltaPos :=  (event position y - bounds top) - delta.
		newPosition := deltaPos / bounds height.
		ann := (TxSliderDragged source: self) oldPosition: sliderPosition; newPosition: newPosition; yourself.
		self sliderPosition: newPosition.
		
		self announcer announce: ann.
	 ].
	

	^ true.