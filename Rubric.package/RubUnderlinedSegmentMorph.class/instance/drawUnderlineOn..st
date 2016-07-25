drawing
drawUnderlineOn: aCanvas
	underlineShape
		do: [ :s | 
			(1 to: s size - 1 by:
				(self straight
					ifTrue: [ 2 ]
					ifFalse: [ 1 ]))
				do: [ :p | 
					| s1 s2 |
					s1 := s at: p.
					s2 := s at: p + 1.
					aCanvas
						line: s1
						to: s2
						width: 2
						color: self underlineColor ] ]