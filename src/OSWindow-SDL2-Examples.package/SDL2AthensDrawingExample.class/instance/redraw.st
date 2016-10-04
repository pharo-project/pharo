updating
redraw
	self clearSurface.
	strokes
		do:
			[ :stroke | 
			| path |
			path := renderer athensSurface
				createPath: [ :builder | 
					builder
						absolute;
						moveTo: stroke first.
					stroke do: [ :point | builder lineTo: point ] ].
			renderer drawDuring: [ :canvas | 
					canvas
						setStrokePaint: Color black;
						setShape: path;
						draw ] ]