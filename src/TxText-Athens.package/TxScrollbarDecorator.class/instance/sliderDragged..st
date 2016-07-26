accessing
sliderDragged: aTxSliderDragged

	| view viewMorph |
	
	viewMorph := viewContainer viewMorph.
	view :=  viewMorph layoutView.
	
	vertical ifFalse: [ 
		| totalSize delta |
		delta := aTxSliderDragged newPosition - aTxSliderDragged oldPosition.
		totalSize := view layout extent x.
		viewMorph scrollBy: (delta * totalSize) @ 0.
		
	 ] ifTrue: [ 
	"vertical one"
		| totalLines approxNewLine clampedPos |
		totalLines := view text endPosition lineNumber.
		clampedPos := aTxSliderDragged newPosition min: 1 max: 0.
		approxNewLine := (totalLines * clampedPos) rounded.

		approxNewLine = 0 ifTrue: [  
			view scrollBy: 0 @ view offset y negated.
			viewMorph resetViewTo: view text startPosition.
			^ self.
			].
	
		view text spansAndLineNumbersDo: [ :span :lineNumber | 
			lineNumber >= approxNewLine ifTrue: [ 
				viewMorph resetViewTo: span startPosition.
				"scrolling up, if layout height is smaller than view height" 
				view height > view layout height ifTrue: [ 
					viewMorph scrollBy: 0 @ (view height - view layout height) negated
					 ].
				^ self.
				].
		].
		self error: 'not reachable.. else it is buggy'.
		view resetLayout: view text endPosition.
		viewMorph scrollBy: (0 @ view extent y negated).
	 ]