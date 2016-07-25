drawing
drawUnderlineOnAthensCanvas: aCanvas
	underlineShape
		do: [ :s | 
			| path |
			(1 to: s size - 1 by:
				(self straight
					ifTrue: [ 2 ]
					ifFalse: [ 1 ]))
				do: [ :p | 
					| s1 s2 |
					s1 := s at: p.
					s2 := s at: p + 1.
					path := aCanvas
						createPath: [ :builder | 
							builder absolute.
							builder moveTo: s1.
							builder lineTo: s2 ].
					(aCanvas setStrokePaint: self underlineColor) width: 1.
					aCanvas drawShape: path ] ]