matching
completeMatch: aKeymap buffer: aBuffer
	KMLog logCompleteMatch: aKeymap.
	KMBuffer uniqueInstance completeMatch.
	aKeymap action cull: target value cull: morph cull: aBuffer first.
