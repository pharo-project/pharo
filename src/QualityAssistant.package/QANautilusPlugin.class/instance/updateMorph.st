announcement handling
updateMorph
	morph ifNil: [ ^ self ].
	
	morph entity: self currentEntity
	