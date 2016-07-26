button styles
grayRoundedButtonStyle: button morph: aMorph pressed: aBoolean
	button useRoundedCorners.
	(self isAlignmentMorph: aMorph)
		ifTrue: [
			aBoolean 
				ifTrue: [	aMorph lastSubmorph color: Color gray.
						button fillStyle: (button theme buttonSelectedFillStyleFor: button).
						button borderStyle: (button theme buttonSelectedBorderStyleFor: button) ]
				ifFalse:[	aMorph lastSubmorph color: Color white.
						button fillStyle: (button theme buttonNormalFillStyleFor: button).
						button borderStyle: (button theme buttonNormalBorderStyleFor: button) ] ]
		ifFalse: [			
			aBoolean 
				ifTrue: [	aMorph color: Color gray.
						button color: Color veryLightGray ]
				ifFalse:[	aMorph color: Color white.
						button color: Color lightGray ] ]
