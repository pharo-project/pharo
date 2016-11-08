*Graphics-Primitives
computeRGBColorConvertingMap: targetColor to: destDepth keepSubPixelAA: keepSubPix 
	"Builds a colormap intended to convert from subpixelAA black values to targetColor values.
	keepSubPix
		ifTrue: [ Answer colors that also include subpixelAA ]
		ifFalse: [ 
			Take fullpixel luminance level. Apply it to targetColor.
			I.e. answer colors with NO subpixelAA ]"
	| mask map c bitsPerColor r g b f v |
	destDepth > 8 
		ifTrue: [ bitsPerColor := 5	"retain maximum color resolution" ]
		ifFalse: [ bitsPerColor := 4 ].
	"Usually a bit less is enough, but make it configurable"
	bitsPerColor := bitsPerColor min: self aaFontsColormapDepth.
	mask := (1 bitShift: bitsPerColor) - 1.
	map := Bitmap new: (1 bitShift: 3 * bitsPerColor).
	0 
		to: map size - 1
		do: 
			[ :i | 
			r := (i bitShift: 0 - (2 * bitsPerColor)) bitAnd: mask.
			g := (i bitShift: 0 - bitsPerColor) bitAnd: mask.
			b := (i bitShift: 0) bitAnd: mask.
			f := 1.0 - ((r + g + b) / 3.0 / mask).
			c := targetColor notNil 
				ifTrue: 
					[ (keepSubPix and: [ destDepth > 8 ]) 
						ifTrue: 
							[ self 
								r: (1.0 - (r / mask)) * targetColor red
								g: (1.0 - (g / mask)) * targetColor green
								b: (1.0 - (b / mask)) * targetColor blue
								alpha: f * targetColor alpha	"alpha will be ignored below, in #pixelValueForDepth: if destDepth ~= 32" ]
						ifFalse: 
							[ destDepth = 32 
								ifTrue: [ targetColor * f alpha: f * targetColor alpha ]
								ifFalse: 
									[ targetColor 
										alphaMixed: f * 1.5
										with: self white ] ] ]
				ifFalse: 
					[ self  
						r: r
						g: g
						b: b
						range: mask ].	"This is currently used only to keep some SubPixelAA on destDepth = 8, using a single pass of rule 25"
			v := destDepth = 32 
				ifTrue: [ c pixelValueForDepth: destDepth ]
				ifFalse: 
					[ f < 0.1 
						ifTrue: [ 0 ]
						ifFalse: [ c pixelValueForDepth: destDepth ] ].
			map 
				at: i + 1
				put: v ].
	^ map