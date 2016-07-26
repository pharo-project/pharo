utils - morph creation
emptyMorph
	| icon |
	icon := (Smalltalk ui icons iconNamed: #emptyIcon) asMorph.
	icon form: (icon form scaledToSize: 16 @ 16).
	^ icon