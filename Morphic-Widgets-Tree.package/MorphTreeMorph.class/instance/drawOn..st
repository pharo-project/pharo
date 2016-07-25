drawing
drawOn: aCanvas 
	super drawOn: aCanvas.
	self columns do: [:col | col drawColumnOn: aCanvas].

	
