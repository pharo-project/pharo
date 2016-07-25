initialize
defaultGraphics

	^ DefaultGraphics ifNil: [ DefaultGraphics := (Form extent: 20 @ 1 depth: 8)].