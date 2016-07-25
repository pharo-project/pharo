private - access
subMorph
	subMorph ifNil: [ self update: self textSelector ].
	^ subMorph