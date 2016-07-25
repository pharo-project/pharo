private
updatePixelPosition
	(xpos := xpos + 1) >= width ifFalse: [ ^ self ].
	xpos := 0.
	interlace ifFalse: 
		[ ypos := ypos + 1.
		^ self ].
	pass = 0 ifTrue: 
		[ (ypos := ypos + 8) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 4 ].
		^ self ].
	pass = 1 ifTrue: 
		[ (ypos := ypos + 8) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 2 ].
		^ self ].
	pass = 2 ifTrue: 
		[ (ypos := ypos + 4) >= height ifTrue: 
			[ pass := pass + 1.
			ypos := 1 ].
		^ self ].
	pass = 3 ifTrue: 
		[ ypos := ypos + 2.
		^ self ].
	^ self error: 'can''t happen'