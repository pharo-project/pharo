Extension { #name : #Color }

{ #category : #'*Graphics-Display Objects' }
Color class >> colorPaletteForDepth: depth extent: chartExtent [
	"Display a palette of colors sorted horizontally by hue and vertically by lightness. Useful for eyeballing the color gamut of the display, or for choosing a color interactively."
	"Note: It is slow to build this palette, so it should be cached for quick access."
	"(Color colorPaletteForDepth: 16 extent: 190@60) display"

	| basicHue x y startHue palette transHt vSteps transCaption grayWidth hSteps |
	palette := Form extent: chartExtent depth: depth.
	transCaption := 
		(Form extent: 34@9 depth: 1
			fromArray: #(0 0 256 0 256 0 3808663859 2147483648 2491688266 2147483648 2491688266 0 2491688266 0 2466486578 0 0 0)
			offset: 0@0).
	transHt := transCaption height.
	palette fillWhite: (0@0 extent: palette width@transHt).
	palette fillBlack: (0@transHt extent: palette width@1).
	transCaption displayOn: palette at: palette boundingBox topCenter - ((transCaption width // 2)@0).
	grayWidth := 10.
	startHue := 338.0.
	vSteps := palette height - transHt // 2.
	hSteps := palette width - grayWidth.
	x := 0.
	startHue to: startHue + 360.0 by: 360.0/hSteps do: [ :h |
		basicHue := Color h: h asFloat s: 1.0 v: 1.0.
		y := transHt+1.
		0 to: vSteps do: [ :n | | c |
 			c := basicHue mixed: (n asFloat / vSteps asFloat) with: self white.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		1 to: vSteps do: [ :n | | c |
 			c := self black mixed: (n asFloat / vSteps asFloat) with: basicHue.
			palette fill: (x@y extent: 1@1) fillColor: c.
			y := y + 1].
		x := x + 1].
	y := transHt + 1.
	1 to: vSteps * 2 do: [ :n | | c |
 		c := self black mixed: (n asFloat / (vSteps*2) asFloat) with: self white.
		palette fill: (x@y extent: 10@1) fillColor: c.
		y := y + 1].
	^ palette
]
