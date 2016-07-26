drawing
drawMoves
	moveToDraw
		do:
			[ :each | 
			| first last moves color path |
			first := each first.
			last := each finish.
			moves := each move.
			color := each color.
			path := renderer athensSurface
				createPath:
					[ :builder | 
					builder
						absolute;
						moveTo: first.
					moves do: [ :move | builder lineTo: move ] ].
			self drawCircleAt: first color: color.
			renderer athensSurface
				drawDuring: [ :canvas | 
					canvas
						setStrokePaint: color;
						setShape: path;
						draw ].
			last ifNotNil: [ self drawCircleAt: last color: color ] ]