geometry
computeSliderBounds
	| max |
	
	max := self height.
		
	mappedThickness := max * thickness max: self minimumThicknessInPixels.
	mappedPosition := (max * sliderPosition) min: max.
	
	mappedPosition + mappedThickness > max ifTrue: [ 
		mappedPosition := (max - mappedThickness).
	]
	 