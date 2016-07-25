drawing
drawCharacter
	renderer
		color: (Color r: 0.0 g: (currentTime*2.0) cos * 0.3 + 0.5 b: 1.0);
		fillRectangle: (characterPosition extent: 20@40)