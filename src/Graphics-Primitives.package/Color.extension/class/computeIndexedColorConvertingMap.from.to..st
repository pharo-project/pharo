*Graphics-Primitives
computeIndexedColorConvertingMap: targetColor from: sourceDepth to: destDepth 
	| map |
	map := (IndexedColors 
		copyFrom: 1
		to: (1 bitShift: sourceDepth)) collect: 
		[ :cc | | f c | 
		f := 1.0 - ((cc red + cc green + cc blue) / 3.0).
		c := targetColor notNil 
			ifTrue: 
				[ destDepth = 32 
					ifTrue: [ targetColor * f alpha: f ]
					ifFalse: 
						[ targetColor 
							alphaMixed: f * 1.5
							with: self white ] ]
			ifFalse: [ cc ].
		destDepth = 32 
			ifTrue: [ c pixelValueForDepth: destDepth ]
			ifFalse: 
				[ f = 0.0 
					ifTrue: [ 0 ]
					ifFalse: [ c pixelValueForDepth: destDepth ] ] ].
	map := map as: Bitmap.
	^ map