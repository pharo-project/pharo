drawing
redraw
	self simulate.
	renderer
		color: Color black;
		clear.
		
	self
		drawCharacter;
		drawBullets