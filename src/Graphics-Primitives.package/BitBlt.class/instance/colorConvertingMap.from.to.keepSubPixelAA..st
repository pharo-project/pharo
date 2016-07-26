private
colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix 
	| srcIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps class == Array ifFalse: [ ColorConvertingMaps := (1 to: 10) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: 
		[ srcIndex := keepSubPix 
			ifTrue: [ 9 ]
			ifFalse: [ 10 ] ].
	mapsForSource := ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest := mapsForSource at: destDepth) ifNil: 
		[ mapsForSourceAndDest := mapsForSource 
			at: destDepth
			put: Dictionary new ].
	map := mapsForSourceAndDest 
		at: targetColor
		ifAbsentPut: 
			[ Color 
				computeColorConvertingMap: targetColor
				from: sourceDepth
				to: destDepth
				keepSubPixelAA: keepSubPix ].
	^ map