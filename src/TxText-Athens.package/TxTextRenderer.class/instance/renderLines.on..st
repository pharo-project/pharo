rendering
renderLines: lines on: aCanvas

	canvas := aCanvas.
	
	canvas pathTransform restoreAfter: [
		lines do: [:line | |  |
			currentLine := line.
			maxAscent := line maxAscent.
			"set the origin to font's baseline position"
			canvas pathTransform translateX: 0 Y: maxAscent.
			"reset advance"
			advance := 0@0.
			
			canvas pathTransform restoreAfter: [
				line renderOn: self ].
			
			
			"line is rendered, advance vertically by line height"
			canvas pathTransform translateX: 0 Y: line height - maxAscent.

"			canvas pathTransform translateX: 0 Y: line height * 1.2 - maxAscent."
		] ]