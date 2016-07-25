matching
verifyMatchWith: anEventBuffer notifying: aListener thenDoing: anAction
	self flag: #fixme. "ugly hack with array of listeners"
	category 
		onMatchWith:  anEventBuffer
		notify: { self . aListener } " first we have to tell the dispatcher "
		andDo: anAction