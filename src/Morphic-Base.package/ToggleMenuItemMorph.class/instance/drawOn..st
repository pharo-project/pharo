drawing
drawOn: aCanvas 
	"Need to check isEnabled here."
	
	self
		isEnabled;
		basicDrawOn: aCanvas;
		drawKeyTextOn: aCanvas