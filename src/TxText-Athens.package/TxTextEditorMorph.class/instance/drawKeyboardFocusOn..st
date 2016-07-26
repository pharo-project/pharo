drawing
drawKeyboardFocusOn: aCanvas 
	"draw an outline, indicating that receiver has the input focus"
	| stroke |
	stroke := aCanvas setStrokePaint: Color yellow  .
	stroke 
		width: self borderWidth;
		joinRound.
	aCanvas
		drawShape: (self innerBounds expandBy: self borderWidth/2.0) ;
		draw.
		