focus
isFocusedMorphVisibleAt: paneIndex
	|morph|
	
	morph := self focusedMorphAt: paneIndex.
	morph ifNil: [ ^ false ].
	^ morph isInWorld.