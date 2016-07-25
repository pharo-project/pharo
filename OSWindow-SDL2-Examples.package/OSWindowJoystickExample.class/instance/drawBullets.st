drawing
drawBullets
	renderer
		color: (Color r: (currentTime * 2.0) cos * 0.4 + 0.6 g: (currentTime * 3.0) sin * 0.4 + 0.6 b: 0.0).
	bullets do: [ :bullet |
		renderer fillRectangle: (bullet first extent: 10@10)
	]