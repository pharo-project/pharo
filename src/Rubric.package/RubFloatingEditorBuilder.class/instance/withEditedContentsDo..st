accessing
withEditedContentsDo: aBlock
	self
		announcerDo:
			[ :anncer | 
			anncer
				when: RubMorphEdited
				send: #value:
				to: [ :ann  | aBlock cull: ann morph text cull: ann morph ] ]