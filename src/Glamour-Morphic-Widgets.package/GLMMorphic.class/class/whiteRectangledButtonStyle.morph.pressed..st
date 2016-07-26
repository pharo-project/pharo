button styles
whiteRectangledButtonStyle: button morph: aMorph pressed: aBoolean

	button useSquareCorners.
	(self isAlignmentMorph: aMorph)
	ifTrue:[ 
		aBoolean 
		ifTrue: [ aMorph lastSubmorph color: Color white.
				      button borderColor: Color gray.
					button color: Color gray ]
		ifFalse:[ aMorph lastSubmorph color: Color black.
			            button borderColor: Color gray.
					button color: Color white ].
		button extent: aMorph width @ (aMorph height - 2).
		aMorph position: 0 @ 0 ]
	ifFalse:[			
		aBoolean 
		ifTrue: [ aMorph color: Color white.
			         button borderColor: Color gray.
				   button color: Color gray ]
		ifFalse:[ aMorph color: Color black.
			            button borderColor: Color gray.
					button color: Color white ].
		button extent: (aMorph width + 6) @ (aMorph height + 2).
		aMorph position: 3 @ 1 ]