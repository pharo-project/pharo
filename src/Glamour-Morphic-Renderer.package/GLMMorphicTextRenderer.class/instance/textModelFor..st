rendering
textModelFor: aPresentation
	^ GLMTextModel new
		text: aPresentation formatedDisplayValue;
		glamourPresentation: aPresentation