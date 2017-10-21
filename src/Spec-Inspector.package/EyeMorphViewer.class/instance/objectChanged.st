event-handling
objectChanged

	self text doItReceiver: self object. 
	self container morph removeAllMorphs.
	self container morph
		addMorph: self thumbnailMorph
		fullFrame: SpecLayoutFrame identity