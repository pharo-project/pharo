miscellaneous
grayColorsFor: d
	"return a color table for a gray image"

	palette := Array new: 1<<d.
	d = 1 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: Color white.
		^  palette
		].
	d = 2 ifTrue: [
		palette at: 1 put: Color black.
		palette at: 2 put: (Color gray: 85.0 / 255.0).
		palette at: 3 put: (Color gray: 170.0 / 255.0).
		palette at: 4 put: Color white.
		^ palette
		].
	d = 4 ifTrue: [
		0 to: 15 do: [ :g |
			palette at: g+1 put: (Color gray: (g/15) asFloat) ].
		^ palette
		].
	d = 8 ifTrue: [
		0 to: 255 do: [ :g |
			palette at: g+1 put: (Color gray: (g/255) asFloat) ].
		^ palette
		].
