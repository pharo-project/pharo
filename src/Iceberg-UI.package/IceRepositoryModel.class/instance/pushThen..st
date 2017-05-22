actions
pushThen: aBlock
	self 
		pushTo: self repository pushRemote 
		then: aBlock