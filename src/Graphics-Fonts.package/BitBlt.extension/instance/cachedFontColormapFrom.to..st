*Graphics-Fonts
cachedFontColormapFrom: sourceDepth to: destDepth 
	"Modified from computeColormapFrom:to:."
	| srcIndex map |
	CachedFontColorMaps class == Array ifFalse: [ CachedFontColorMaps := (1 to: 9) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: [ srcIndex := 9 ].
	(map := (CachedFontColorMaps at: srcIndex) at: destDepth) ~~ nil ifTrue: [ ^ map ].
	map := (Color 
		cachedColormapFrom: sourceDepth
		to: destDepth) copy.
	(CachedFontColorMaps at: srcIndex) 
		at: destDepth
		put: map.
	^ map