*Graphics-Primitives
cachedColormapFrom: sourceDepth to: destDepth 
	"Return a cached colormap for mapping between the given depths. Always return a real colormap, not nil; this allows the client to get an identity colormap that can then be copied and modified to do color transformations."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!"
	"Note: The colormap cache may be cleared by evaluating 'Color shutDown'."
	| srcIndex map |
	CachedColormaps class == Array ifFalse: [ CachedColormaps := (1 to: 9) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: [ srcIndex := 9 ].
	(map := (CachedColormaps at: srcIndex) at: destDepth) ~~ nil ifTrue: [ ^ map ].
	map := self 
		computeColormapFrom: sourceDepth
		to: destDepth.
	(CachedColormaps at: srcIndex) 
		at: destDepth
		put: map.
	^ map