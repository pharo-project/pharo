converting
asCursorForm
	| form |
	form := Form extent: self extent depth: 8.
	form fillShape: maskForm fillColor: Color white.
	form fillShape: self fillColor: Color black at: offset negated.
	^ form offset: offset