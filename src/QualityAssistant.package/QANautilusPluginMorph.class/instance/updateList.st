actions
updateList

	showPrivacyMorph ifTrue: [ ^ self ].
	
	self removeAllMorphs.
	self addMorphFront: progressMorph.
	self excecuteTask