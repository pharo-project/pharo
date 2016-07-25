protocol
updateMorphs

	self removeAllMorphs.
	
	showOptions
		ifTrue: [ self addMorphsWithOptions ]
		ifFalse: [ self addMorphsWithoutOptions ]