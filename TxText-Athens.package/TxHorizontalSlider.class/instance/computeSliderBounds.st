geometry
computeSliderBounds
	| max |
	
	max := self width.
		
	mappedThickness := max * thickness max: self minimumThicknessInPixels.
	mappedPosition := max * sliderPosition min: max.
	
	mappedPosition + mappedThickness > max ifTrue: [ 
		mappedPosition := max - mappedThickness.
		 ]
	 