updating
adjustSubmorphPositionsOf: aCollection startIdx: anIndex startPos: aStartPoint
	| p idx |
	p := aStartPoint.
	idx := anIndex.
	aCollection do: [ :each | | h |
		h := each height.
		each index: idx.
		each bounds: (p extent: each width @ h).
		maxNodeWidth := maxNodeWidth max: (each fullBounds width).
		idx := idx + 1.
		p := p + (0@h)].
	
	self setScrollDeltas.
	^ p
	